
library(shiny)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(DT)
library(leaflet)
library(lubridate)
library(RColorBrewer)

#destination <- file.choose()

my_df <- readRDS("isbike_20201118.rds")
json_df <- fromJSON(my_df)
final_df <- json_df[["dataList"]]


isbike_df <- final_df %>% 
    transmute(StationNo = as.integer(istasyon_no), 
              StationName = adi, 
              Available = as.integer(bos),
              Occupied = as.integer(dolu),
              Capacity = Available + Occupied,
              AvailabilityRate = round((Available / Capacity * 100), 1),
              Latitude = as.numeric(lat),
              Longtitude = as.numeric(lon),
              LastConnection = as.POSIXct(sonBaglanti,format='%Y-%m-%dT%H:%M:%S'),
              LastConnectionDay = day(LastConnection)) %>%
    mutate(AvailabilityRate = replace(AvailabilityRate, is.na(AvailabilityRate), 0),
           Latitude = replace(Latitude, is.na(Latitude), 0))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("isbike Stations"),
    
    tabsetPanel(
        tabPanel("General Information", 
                 leafletOutput("isbikeMap"),
                 #plotOutput("isbikeHist"), 
                 plotOutput("isbikeCapacity")),
        tabPanel("Current Availability",

    # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    sliderInput("available",
                                "Available Bikes:",
                                min = min(isbike_df$Available),
                                max = max(isbike_df$Available),
                                value = c(min(isbike_df$Available)+1, max(isbike_df$Available)-1),
                                step = 1),
                    sliderInput("availability",
                                "Availability Rate %:",
                                min = min(isbike_df$AvailabilityRate),
                                max = max(isbike_df$AvailabilityRate),
                                value = c(min(isbike_df$AvailabilityRate)+1, max(isbike_df$AvailabilityRate)-1),
                                step = 5),
                    
                ),
        
                # Show a plot of the generated distribution
                mainPanel(leafletOutput("leafletMap"), DTOutput("isbikeTable"))
                )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$isbikeMap <- renderLeaflet({

        map_df <- isbike_df %>%
            filter(Longtitude != 0 & Latitude != 0)

        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircleMarkers(lng = map_df$Longtitude, lat = map_df$Latitude,
                             weight = 5, radius = 3,
                             popup = paste0(map_df$StationName,
                                            "<br/>Total Capacity: ", map_df$Capacity,
                                            "<br/>Available Bikes: ", map_df$Available,
                                            "<br/>Occupied Bikes: ", map_df$Occupied,
                                            "<br/>Last Connection: ", map_df$LastConnection))
        
    })
    
    output$isbikeCapacity <- renderPlot({
        
        plot_df <- isbike_df %>% 
            filter(Capacity > 0) %>%
            count(Capacity, name = "Count") %>%
            mutate(Percentage = round(Count / sum(Count) * 100, 0)) %>%
            mutate(Capacity = as.character(Capacity)) %>%
            select(Capacity, Percentage)
            
        ggplot(plot_df, aes(x = reorder(Capacity, Percentage), y = Percentage)) + 
            geom_bar(stat = "identity", aes(fill=Capacity)) + 
            coord_flip() +
            labs(title = "Capacity Distribution of Stations", x = "Capacity", y = "") + 
            theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        
        
    })
    
    output$leafletMap <- renderLeaflet({
        
        leaflet_df <- isbike_df %>% 
            filter(Longtitude != 0 & Latitude != 0) %>%
            filter(AvailabilityRate >= input$availability[1], 
                   AvailabilityRate <= input$availability[2]) %>%
            filter(Available >= input$available[1], 
                   Available <= input$available[2]) %>%
            mutate(AvailableFactor = factor(Available))
        
        color_vec <- brewer.pal(n = 11, name = "RdYlGn")

        new <- color_vec[leaflet_df$AvailableFactor]
        new <- replace(new, is.na(new), "#006837")

        icons <- awesomeIcons(
            icon = "bicycle",
            iconColor = "white",
            library = "ion"
        )
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addAwesomeMarkers(lng = leaflet_df$Longtitude, lat = leaflet_df$Latitude,
                              icon = icons,
                              popup = paste0(leaflet_df$StationName,
                                             "<br/>Total Capacity: ", leaflet_df$Capacity,
                                             "<br/>Available Bikes: ", leaflet_df$Available,
                                             "<br/>Occupied Bikes: ", leaflet_df$Occupied,
                                             "<br/>Last Connection: ", leaflet_df$LastConnection))
        
    })  
    
    output$isbikeTable <- renderDT({
        
        table_df <- isbike_df %>% 
            filter(Available >= input$available[1], Available <= input$available[2]) %>%
            filter(AvailabilityRate >= input$availability[1], AvailabilityRate <= input$availability[2]) %>%
            select(StationNo, StationName, Available, Occupied, Capacity, AvailabilityRate)
        
        table_df
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

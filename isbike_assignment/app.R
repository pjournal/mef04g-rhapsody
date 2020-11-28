
library(shiny)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(sf)
library(mapview)
library(DT)
library(leaflet)
library(lubridate)

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
    mutate(AvailabilityRate = replace(AvailabilityRate, is.na(AvailabilityRate), 0))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("isbike Stations"),
    
    tabsetPanel(
        tabPanel("General Information", 
                 mapviewOutput("isbikeMap"), 
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
                mainPanel(DTOutput("isbikeTable"))
                )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$isbikeMap <- renderMapview({
        
        non_na_df <- isbike_df
        
        non_na_df$Latitude[non_na_df$Latitude == ""] = "0"
        non_na_df$Longtitude[non_na_df$Longtitude == ""] = "0"

        map_df <- non_na_df %>%
            #filter(Available >= input$available[1]) %>%
            #filter(Available <= input$available[2]) %>%
            filter(!Longtitude %in% "0") %>%
            filter(!Latitude %in% "0") %>%
            select(StationName, Longtitude, Latitude)

         isbike_map_sf <- st_as_sf(map_df, coords = c("Longtitude", "Latitude"), crs = 4326)
         mapview(isbike_map_sf, legend = FALSE)
        
    })
    
    # output$isbikeHist <- renderPlot({
    #     
    #     hist_vector <- isbike_df %>% 
    #         transmute(TotalCapacity = Available + Occupied)
    #     
    #     hist_vector <- as.numeric(unlist(hist_vector))
    #         
    #     
    #     bins <- seq(min(hist_vector), max(hist_vector), length.out = 30)
    #     
    #     # draw the histogram with the specified number of bins
    #     hist(hist_vector, breaks = bins, col = 'darkgray', border = 'white')
    #     
    # })
    
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

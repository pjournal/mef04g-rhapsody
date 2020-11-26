#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(tidyverse)
# library(jsonlite)
# library(ggplot2)
# library(sf)
# library(mapview)
# library(DT)
# library(leaflet)

#destination <- file.choose()
#my_df <- readRDS(destination)
#json_df <- fromJSON(my_df)
#isbike_df <- json_df[["dataList"]]
# 
# isbike_df <- isbike_df %>% 
#     transmute(StationName = adi, Available = bos, Occupied = dolu, Latitude = lat, Longtitude = lon)
#     
# isbike_df$Available = as.integer(isbike_df$Available)
# isbike_df$Occupied = as.integer(isbike_df$Occupied)
#     
# 
# 
# deneme <- isbike_df
# deneme$Latitude[deneme$Latitude == ""] = "0"
# deneme$Longtitude[deneme$Longtitude == ""] = "0"
# 
# deneme <-  deneme %>% 
#     filter(!Longtitude %in% "0") %>%
#     filter(!Latitude %in% "0")
# 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("isbike Stations"),
    
    tabsetPanel(
        tabPanel("General Information", mapviewOutput("isbikeMap"), plotOutput("isbikeHist")),
        tabPanel("Current Availability",

    # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    sliderInput("available",
                                "Available Bikes:",
                                min = min(isbike_df$Available),
                                max = max(isbike_df$Available),
                                value = c(min(isbike_df$Available)+1, min(isbike_df$Available)+2),
                                step = 1)
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
    
    output$isbikeHist <- renderPlot({
        
        hist_vector <- isbike_df %>% 
            transmute(TotalCapacity = Available + Occupied)
        
        hist_vector <- as.numeric(unlist(hist_vector))
            
        
        bins <- seq(min(hist_vector), max(hist_vector), length.out = 30)
        
        # draw the histogram with the specified number of bins
        hist(hist_vector, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
    output$isbikeTable <- renderDT({
        
        table_df <- isbike_df %>% 
            filter(Available >= input$available[1]) %>%
            filter(Available <= input$available[2]) %>%
            select(StationName, Available, Occupied)
        
        table_df
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

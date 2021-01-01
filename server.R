library(shiny)
setwd("C:/Users/apina/OneDrive/Escritorio/MASTER/UPM/BD/Visualization project/project/happiness app")
#install.packages("leaflet")
library(leaflet)
library(dplyr)
#install.packages("leaflet.extras")
library(leaflet.extras)

# import data: 
data <- read.csv("2019.csv")

# Categorize score

data$Score_type <- ifelse(data$Score <= 2.5, "Low Score", 
              ifelse(data$Score <= 5  | data$Score >2.5, "Medium Score", 
              ifelse(data$depth > 7.5, "High Score", "other")))


# --------- UI --------------------------

ui <- fluidPage(
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view score groups
    absolutePanel(top = 60, left = 20, 
                  checkboxInput("markers", "Score", FALSE),
                  checkboxInput("heat", "Heatmap", FALSE)
    )
  ))


# ---------- SERVER -------------------


server <- function(input, output, session) {
  #define the color pallate for the group of Score
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$Score)
  
  #define the color of for the number of score
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = data$Score_type
  )
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>% 
      addCircles(data = data, lat = 10, lng = 20, weight = 1, radius = ~sqrt(data$Score)*25000, popup = ~as.character(data$Score), label = ~as.character(paste0("Score: ", sep = " ", data$Score)), color = ~pal(data$Score), fillOpacity = 0.5)
  })
  
}


shinyApp(ui, server)
                     
# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  list_regions <- list('All', 
                       'Western Europe',
                       "North America",
                       'Africa',
                       'Asia'),
  
  titlePanel("Interactive Visualization Tool for Happiness Analysis"),
  navbarPage("Choose Plot Type:",
             tabPanel("Spatial Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Interaction Panel"),
                          selectInput("region", "Choose a region:",
                                      list_regions
                          ),
                          
                          sliderInput("Year",
                                      "Select the year-range:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015, 2019),
                                      dragRange=TRUE)
                        ),
                        mainPanel(plotlyOutput("plot"))
                        
                      )),
             
             tabPanel("Relationship Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Interaction Panel"),
                          selectInput("region2", "Choose a region:",
                                      list_regions
                          ),
                          
                          sliderInput("Year",
                                      "Select the year-range:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015, 2019),
                                      dragRange=TRUE)
                        ),
                        mainPanel(plotlyOutput("plot2")))),
             
             tabPanel("Plot 3", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Interaction Panel"),
                          selectInput("region", "Choose a region:",
                                      list('All', 
                                           'Europe',
                                           'Africa',
                                           'Asia')
                          ),
                          
                          sliderInput("Year",
                                      "Select the year-range:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015, 2019),
                                      dragRange=TRUE)
                        ),
                        mainPanel(plotlyOutput("plot3")))
                      )
  ),
)

# Server logic
server <- function(input, output) {
  output$plot <- renderPlotly(scatter_matrix(input$region))
  output$plot2 <- renderPlotly(plot_relationships(input$region2))
}

# Run the app
shinyApp(ui, server)

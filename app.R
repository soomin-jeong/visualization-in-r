# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")

list_regions <- list('All', 
                     'Europe',
                     "America",
                     'Africa',
                     'Asia',
                     'Oceanico')

list_countries <- list()

# User interface ----
ui <- fluidPage(
  
  titlePanel("Interactive Visualization Tool for Happiness Analysis"),
  navbarPage("Choose Plot Type:",
             tabPanel("Spatial Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          h4("Interaction Panel"),
                          selectInput("region", "Choose a region:",
                                      list_regions
                          ),
                          
                          
                          sliderInput("Year1",
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
                          h4("Interaction Panel"),
                          selectInput("region2", "Choose a region:",
                                      list_regions
                          ),
                          
                          selectInput("country", "Choose a country to highlight:",
                                      choices = c()
                          ),
                          
                          sliderInput("Year2",
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
                          h4("Interaction Panel"),
                          selectInput("region", "Choose a region:",
                                      list('All', 
                                           'Europe',
                                           'Africa',
                                           'Asia')
                          ),
                          
                          sliderInput("Year3",
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
server <- function(input, output, session) {
  myData <- reactive({
    selectData(input$region, input$Year1, input$country)
  })
  
  observe({
    x <- sort(unique(myData()$Country))
    # Can also set the label and select items
    updateSelectInput(session, "country",
                      choices = x,
                      selected = ""
    )
  })
  #output$plot <- renderPlotly()
  output$plot2 <- renderPlotly(plot_relationships(input$region2, input$Year2, input$country))
}

# Run the app
shinyApp(ui, server)

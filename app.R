# Load packages ----
library(shiny)
library(quantmod)
library(ECharts2Shiny)

# Source helpers ----
source("helpers.R")

list_regions <- list('All', 
                     'Europe',
                     "America",
                     'Africa & Middle East',
                     'Asia',
                     'Oceanico')

list_countries <- list()

list_colors <- list("Reds", "Blues", "Oranges")

# User interface ----
ui <- fluidPage(
  
  titlePanel("Interactive Visualization Tool for Happiness Analysis"),
  navbarPage("Choose Plot Type:",
             tabPanel("Spatial Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          h4("Interaction Panel"),
                          selectInput("region1", "Choose a region:",
                                      list_regions
                          ),
                          
                          selectInput("color1", "Choose the color map:",
                                      choices = list_colors
                          ),
                          
                          sliderInput("Year1",
                                      "Select the year-range:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015, 2019),
                                      dragRange=TRUE)
                        ),
                        mainPanel(plotlyOutput("plot1"), 
                                  HTML("<footer>
                                       By Tom van Knippenberg, Soo Min Jeong and Arturo Piñar Adan
                                       </footer>"))
                        
                      )),
             
             tabPanel("Relationship Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          h4("Interaction Panel"),
                          selectInput("region2", "Choose a region:",
                                      list_regions
                          ),
                          
                          selectInput("country2", "Choose a country to highlight:",
                                      choices = c()
                          ),
                          
                          selectInput("color2", "Choose the color map:",
                                      choices = list_colors
                          ),
                          
                          sliderInput("Year2",
                                      "Select the year-range:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015, 2019),
                                      dragRange=TRUE)
                        ),
                        mainPanel(plotlyOutput("plot2"),
                                  HTML("<footer>
                                       By Tom van Knippenberg, Soo Min Jeong and Arturo Piñar Adan
                                       </footer>")))),
             
             tabPanel("Happiness Evolution Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          h4("Interaction Panel"),
                          selectInput("region3", "Choose a region:",
                                      list_regions
                          ),
                          
                          selectInput("country3", "Choose a country to highlight:",
                                      choices = c()
                          ),
                          
                          # selectInput("color3", "Choose the color map:",
                          #             choices = list_colors
                          # ),
                          # 
                          # sliderInput("Year3",
                          #             "Select the year-range:",
                          #             min = 2015,
                          #             max = 2019,
                          #             value= c(2015, 2019),
                          #             dragRange=TRUE)
                        ),
                        mainPanel(plotlyOutput("plot3"),
                                  HTML("<footer>
                                       By Tom van Knippenberg, Soo Min Jeong and Arturo Piñar Adan
                                       </footer>")))
                      )
  ),
)

# Server logic
server <- function(input, output, session) {
  # Tab 1
  #output$plot1 <- renderPlotly()
  
  # Tab 2
  myData <- reactive({
    selectData(input$region2, input$Year2, input$country2, input$color2)
  })
    
  observe({
    x <- input$region2
    data <- selectData(input$region2, input$Year2, input$country2, input$color2)
    updateSelectInput(session, "country2",
                      choices = c("None", sort(unique(data$Country))),
                      selected = input$country2
    )
  })
  
  output$plot2 <- renderPlotly(plot_relationships(input$region2, input$Year2, input$country2,
                                                  input$color2))
  # Tab 3
  observe({
    x <- input$region3
    data <- selectData(input$region3, c(2015, 2019), input$country3, "Reds")
    updateSelectInput(session, "country3",
                      choices = c("None", sort(unique(data$Country))),
                      selected = input$country3
    )
  })
  output$plot3 <- renderPlotly(line_chart(input$region3, c(2015, 2019), input$country3, "Reds"))
}

# Run the app
shinyApp(ui, server)

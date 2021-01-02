setwd("/Users/JeongSooMin/Documents/workspace/visualization-in-r/happiness_report_shiny_app")
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel(h1("Happiness Around the Globe")),
  fluidRow(
    column(12,
           helpText("In this graph, you can see the distribution of the global happiness including 151 countries")
    )
  ),
  sidebarLayout(
    sidebarPanel(
                checkboxGroupInput("countries", 
                                   h3("Countries"), 
                                   choices = list("Spain" = "Spain", 
                                                  "Korea" = "Korea", 
                                                  "USA" = "USA")
                                   ),
                sliderInput("date_range", 
                            h3("Date Range"),
                            min = as.Date("2015-01-01","%Y-%m-%d"),
                            max = as.Date("2018-12-01","%Y-%m-%d"),
                            value=c(as.Date("2016-01-01"), as.Date("2017-12-01")),
                            timeFormat="%Y-%m-%d"
                            )
                 ),
    mainPanel(
      textOutput("selected_var")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_var <- renderText({
    paste("You have countries: ", c(input$countries), "&",
          "You have range:", input$date_range[1], input$date_range[2])
  })

}

shinyApp(ui = ui, server = server)


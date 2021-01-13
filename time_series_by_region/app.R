## times_series_by_region

setwd("/Users/JeongSooMin/Documents/workspace/visualization-in-r/time_series_by_region")

source("helpers.R")

library(shiny)

data <- read.csv("../data/2015.csv")
colnames(data)
data$Happiness.Score


# Q3: What is the evolution of happiness over time? 

ui <- fluidPage(
  titlePanel(h1("Happiness By Region", align="center")),
  fluidRow(
    column(12,
           helpText("You can see the chronical change of happiness by region")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("region", 
                         h3("Region"), 
                         choices = list("Sub-Saharan Africa" = "Sub-Saharan Africa", 
                                        "Central and Eastern Europe" = "Central and Eastern Europe")
      ),
      checkboxGroupInput("countries", 
                         h3("Countries"), 
                         choices = country_names
      )
    ),
    mainPanel(
      plotOutput("bar", height=500)
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$bar <- renderPlot({
    color <- c("blue", "red")
    barplot(colSums(data[,c("Happiness.Score")]),
            ylab="Total",
            xlab="Census Year",
            names.arg = c("Happiness.Score"),
            col = color)
  })
}

shinyApp(ui = ui, server = server)


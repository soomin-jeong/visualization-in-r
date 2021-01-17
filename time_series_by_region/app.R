## times_series_by_region

setwd("/Users/JeongSooMin/Documents/workspace/visualization-in-r/time_series_by_region")

source("helpers.R")

library(dplyr)
library(shiny)
library(tidyverse)
library(ECharts2Shiny)

# Q3: What is the evolution of happiness over time? 

joined_data

ui <- fluidPage(
  loadEChartsLibrary(),
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
      tags$div(id="linechart", style="width:100%;height:1000px;"),
      deliverChart(div_id = "linechart")    
      )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$linechart <- renderLineChart("linechart", t(joined_data), theme = "default", running_in_shiny = TRUE)
}

shinyApp(ui = ui, server = server)


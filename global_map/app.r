# Global Map
setwd("/Users/JeongSooMin/Documents/workspace/visualization-in-r/global_map")

data <- read.csv("../data/2015.csv")

# source("helpers.R")

# Q3: What is the evolution of happiness over time? 
library(shiny)
library(highcharter)
library(countrycode)
library(shinydashboard)
library(dplyr)
library(openintro)

data <- read.csv("../data/2015.csv")
colnames(data)

ui<-
  dashboardPage(
    dashboardHeader(title = "Happiness Score"),
    dashboardSidebar(
      
      sidebarMenu( 
        # TODO : add up to 2019
        selectInput('yearid','Select Year for Global Happiness Score',choices = c(2015),selected = 2015)
      )),
    dashboardBody(
      tabBox(title = 'Global Happiness Map',id = 'tabset1',width = 12, tabPanel('Happiness Score',highchartOutput('chart',height = '500px')))
      
    )
  )



server <- function(input, output, session){
  score <- reactive(
    {
      data %>%
        mutate(iso3 = countrycode(Country,"country.name","iso3c")) %>%
        mutate(score = Happiness.Score)
       ## TO BE ADDED WITH THE DATASET IN OTHER YEARS
       ##filter(YEAR_ID == as.numeric(input$yearid)) %>% 
    }
  )
  
  output$chart <- renderHighchart(highchart(type = "map") %>% 
                                    hc_add_series_map(map = worldgeojson, df = score(), value = "score", joinBy = "iso3") %>% 
                                    hc_colorAxis(stops = color_stops()) %>% 
                                    hc_tooltip(useHTML=TRUE,headerFormat='',pointFormat = paste0(input$yearid,'  {point.Country} score : {point.score} ')) %>% 
                                    hc_title(text = 'Global Hapiness Score') %>% 
                                    hc_subtitle(text = paste0('Year: ',input$yearid)) %>% 
                                    hc_exporting(enabled = TRUE,filename = 'custom')
  )
  observeEvent(input$yearid,{
    updateTabItems(session,'tabset1')
  })
}

shinyApp(ui=ui, server=server)

setwd("C:/Users/apina/OneDrive/Escritorio/MASTER/UPM/BD/Visualization project/project/happiness app")

data <- read.csv("2019.csv")

# Q3: What is the evolution of happiness over time? 



colnames(data)

if(!require(shiny)) install.packages("shiny")
if(!require(highcharter)) install.packages("highcharter")
if(!require(countrycode)) install.packages("countrycode")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(dplyr)) install.packages("dplyr")
if(!require(openintro)) install.packages("openintro")

library(shiny)
library(highcharter)
library(countrycode)
library(shinydashboard)
library(dplyr)
library(openintro)

# ToDo: Combine with Soo plot.
# ToDo: If we consider to filter by "Score", "GDP per capita", "Health"... 

ui<-
  dashboardPage(
    dashboardHeader(title = "Spatial Distribution of Happiness"),
    dashboardSidebar(
      
      sidebarMenu( 
        selectInput('variable','Select Variable you want to see',choices = c("Overall rank", "Happiness score", 
                                                                             "Economy (GDP per capita)", "Social Support", 
                                                                             "Health (life expectancy)", "Freedom to make life choices",
                                                                             "Perceptions of corruption", "Generosity"),selected = "Happiness score")
      )),
    dashboardBody(
      tabBox(title = 'Global Happiness ',id = 'tabset1',width = 12, tabPanel('Happiness Distribution',highchartOutput('chart',height = '500px')))
      
    )
  )


server <- function(input, output, session){
  score <- reactive(
    {
      data %>%
        mutate(iso3 = countrycode(Country,"country.name","iso3c")) %>%
        mutate(score = as.numeric(Happiness.Score))
    }
  )
  
  output$chart <- renderHighchart(highchart(type = "map") %>% 
                                    hc_add_series_map(map = worldgeojson, df = score(), value = "Happiness.Distribution", joinBy = "iso3") %>% 
                                    hc_colorAxis(stops = color_stops()) %>% 
                                    hc_tooltip(useHTML=TRUE,headerFormat='',pointFormat = paste0(input$variable,'  {point.Country} score : {point.score}')) %>% 
                                    hc_title(text = 'Global Hapiness Distribution') %>% 
                                    hc_subtitle(text = paste0('Variable: ',input$variable)) %>% 
                                    hc_exporting(enabled = TRUE,filename = 'custom')
  )
  observeEvent(input$variable,{
    updateTabItems(session,'tabset1')
  })
}

shinyApp(ui=ui, server=server)


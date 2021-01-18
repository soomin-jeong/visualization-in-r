
setwd("C:/Users/apina/OneDrive/Escritorio/MASTER/UPM/BD/Visualization project/project/happiness app")

if(!require(dplyr)) install.packages("dplyr")
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(countrycode)) install.packages("countrycode")

library(dplyr)
library(shiny)
library(plotly)
library(countrycode)





# Import data
happiness_data <- read.csv("../composed dataset/FullData.csv")

# UI
ui <- fluidPage(
  # Navbar structure for UI
  navbarPage("Happiness Distribution",
             tabPanel("Score", icon = icon("globe-americas"),
                      # Sidebar layout with an input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          # Select Year
                          selectInput(inputId = "yearId",
                                      label = "Select the year desired:",
                                      choices = c("2019", "2018", "2017", "2016", "2015"),
                                      selected = "2019",
                                      width = "220px"
                          )
                          
                          
                        ),
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,
                                   radioButtons(inputId = "variableSelectorId",
                                                label = "Variable: ",
                                                choices = c("Happiness Score", "Rank", "Economy (GDP per capita)", "Family", "Health",
                                                            "Freedom", "Government corruption", "Generosity"),
                                                selected = "Happiness Score")
                            )),
                          plotlyOutput("map", width = "1400px")
                          
                        )
                      )
             )
  )
)


plot_map <- function (dataTarget, dataTargetColumn, color, legendTitle, plotTitle) {
  l <- list(color = toRGB("grey"), width = 0.5)
  
  fig <- plot_geo(dataTarget)
  fig <- fig %>% add_trace(
    z = dataTargetColumn, color = dataTargetColumn, colors = color,
    text = dataTarget$Country, locations = dataTarget$countriesCodes, marker = list(line = l)
  )
  fig <- fig %>% colorbar(title = legendTitle)
  fig <- fig %>% layout(
    title = plotTitle
  )
  
  fig
}




# Server
server <- function (input, output, session) {
  
  
  yearData <- reactive(
    {
     happiness_data %>%
        filter(as.numeric(Year) == input$yearId) %>%
        mutate(countriesCodes = countrycode(Country, "country.name", destination = "iso3c")) %>%
        mutate(HappinessScore = as.numeric(round(HappinessScore, digits = 2))) %>%
        mutate(Economy = as.numeric(round(Economy, digits = 2))) %>%
        mutate(Family = as.numeric(round(Family, digits = 2))) %>%
        mutate(Health = as.numeric(round(Health, digits = 2))) %>%
        mutate(Freedom = as.numeric(round(Freedom, digits = 2))) %>%
        mutate(GovernmentCorruption = as.numeric(round(GovernmentCorruption, digits = 2))) %>%
        mutate(Generosity = as.numeric(round(Generosity, digits = 2)))
        
      
    }
  )
  
  output$map <- renderPlotly({
    
    data <- yearData()
    
    columnDataTarget <- switch(input$variableSelectorId,
                               "Happiness Score" = data$HappinessScore,
                               "Rank" = data$Rank,
                               "Economy (GDP per capita)" = data$Economy,
                               "Family" = data$Family,
                               "Health" = data$Health,
                               "Freedom" = data$Freedom,
                               "Government corruption" = data$GovernmentCorruption,
                               "Generosity" = data$Generosity)
    
    colors <- switch(input$variableSelectorId,
                     "Happiness Score" = "Reds",
                     "Rank" = "Blues",
                     "Economy (GDP per capita)" = "Greens",
                     "Family" = "Purples",
                     "Health" = "Oranges",
                     "Freedom" = c("#F20AFA", "#440C46"),
                     "Government corruption" = "Greys",
                     "Generosity" = c("#10F7F7", "#053737"))
    
    plotTitle <- switch(input$variableSelectorId,
                          "Happiness Score" = "Happiness Score:",
                          "Rank" = "Happiness Rank: ",
                          "Economy (GDP per capita)" = "Economy (GDP per capita): ",
                          "Family" = "Family (extent to which family contributes to the calculation of happiness score): ",
                          "Health" = "Health (Life expectancy): ",
                          "Freedom" = "Freedom to make life choices: ",
                          "Government corruption" = "Government corruption: ",
                          "Generosity" = "Generosity: ")
    legendTitle <- switch(input$variableSelectorId,
                        "Happiness Score" = "Happiness Score:",
                        "Rank" = "Happiness Rank: ",
                        "Economy (GDP per capita)" = "Economy: ",
                        "Family" = "Family: ",
                        "Health" = "Health: ",
                        "Freedom" = "Freedom: ",
                        "Government corruption" = "Government corruption: ",
                        "Generosity" = "Generosity: ")
    
    
    plot_map(data, columnDataTarget, colors, legendTitle, plotTitle)
    
  })

}

shinyApp(ui = ui, server = server)             




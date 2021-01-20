# Check packages ---
if(!require(dplyr)) install.packages("dplyr")
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(countrycode)) install.packages("countrycode")

# Load packages ----
library(shiny)
library(quantmod)
library(dplyr)
library(countrycode)


# Source helpers ----
source("helpers.R")

list_regions <- list('All', 
                     'Europe',
                     "America",
                     'Africa & Middle East',
                     'Asia',
                     'Oceania')

list_countries <- list()

list_colors <- list("Reds", "Blues", "Oranges", "Greens")

footer <- "<footer> By Soo Min Jeong, Tom van Knippenberg and Arturo Pinar Adán
                                       </footer>"
width_interaction <- 3
width_main <- 9

# User interface ----
ui <- fluidPage(
  
  titlePanel("Interactive Visualization Tool for Happiness Analysis"),
  navbarPage("Choose Plot Type:",
             tabPanel("Spatial Plot", 
                      sidebarLayout(
                        sidebarPanel(width= width_interaction,
                          h4("Interaction Panel"),
                          # selectInput("region1", "Choose a region:",
                          #             list_regions
                          # ),
                          
                          selectInput("color1", "Choose the color map:",
                                      choices = list_colors
                          ),
                          
                          radioButtons(inputId = "variableSelectorId",
                                       label = "Select variable to plot: ",
                                       choices = c("Happiness Score", "Economy (GDP per capita)", "Social Support", "Health",
                                                   "Freedom", "Government corruption", "Generosity"),
                                       selected = "Happiness Score"),
                          
                          sliderInput("Year1",
                                      "Select the year:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015),
                                      dragRange=FALSE)
                        ),
                        mainPanel(width = width_main,
                          h2("Spatial Distribution of Data"),
                          fluidRow(column(1,
                          plotlyOutput("plot1", width = "60vw", height = "60vh"))), 
                                  HTML(footer))
                        
                      )),
             
             tabPanel("Relationship Plot", 
                      sidebarLayout(
                        sidebarPanel(width= width_interaction,
                          h4("Interaction Panel"),
                          selectInput("region2", "Choose a region:",
                                      list_regions
                          ),
                          
                          selectInput("country2", "Choose a country to highlight:",
                                      choices = c()
                          ),
                          
                          selectInput("color2", "Choose the color map:",
                                      choices = list_colors,
                                      selected = "Reds"
                          ),
                          
                          sliderInput("Year2",
                                      "Select the year-range:",
                                      min = 2015,
                                      max = 2019,
                                      value= c(2015, 2019),
                                      dragRange=TRUE),
                          
                          sliderInput("Opac2",
                                      "Select the marker opacity:",
                                      min = 0,
                                      max = 1,
                                      value= c(1),
                                      dragRange=TRUE)
                        ),
                        mainPanel(width = width_main,
                          h2("Heatmap of Correlations and Scatterplot for Relationship with Happiness"),
                          plotlyOutput("plot2"),
                                  HTML(footer)))),
             
             tabPanel("Happiness Evolution Plot", 
                      sidebarLayout(
                        sidebarPanel(width= width_interaction,
                          h4("Interaction Panel"),
                          selectInput("region3", "Choose a region:",
                                      list_regions
                          ),
                          
                          selectInput("country3", "Choose a country to show:",
                                      choices = c()
                          ),
                          
                          # selectInput("color3", "Choose the color map:",
                          #             choices = list_colors
                          # ),
                          
                          checkboxInput(inputId = "jux3",
                                       label = "Juxtaposition on Regions")
                        ),
                        mainPanel(width = width_main,
                          h2("Line Chart with Evolution of Happiness over Time"),
                          plotlyOutput("plot3"),
                                  HTML(footer)))
                      )
  ),
)

# Server logic
server <- function(input, output, session) {
  # Tab 1
  yearData <- reactive(
    {selectData("All", input$Year1, "", "Reds") %>%
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
  
  output$plot1 <- renderPlotly({
  
  data <- yearData()
  
  columnDataTarget <- switch(input$variableSelectorId,
                             "Happiness Score" = data$HappinessScore,
                             "Economy (GDP per capita)" = data$Economy,
                             "Social Support" = data$Family,
                             "Health" = data$Health,
                             "Freedom" = data$Freedom,
                             "Government corruption" = data$GovernmentCorruption,
                             "Generosity" = data$Generosity)
  
  colors <- switch(input$variableSelectorId,
                   "Happiness Score" = "Reds",
                   "Economy (GDP per capita)" = "Greens",
                   "Social Support" = "Purples",
                   "Health" = "Oranges",
                   "Freedom" = "Blues", #c("#F20AFA", "#440C46"),
                   "Government corruption" = "Greys",
                   "Generosity" = c("#10F7F7", "#053737"))
  
  plotTitle <- switch(input$variableSelectorId,
                      "Happiness Score" = "Happiness Score",
                      "Economy (GDP per capita)" = "Economy (GDP per capita)",
                      "Social Support" = "Social Support",
                      "Health" = "Health (Life expectancy)",
                      "Freedom" = "Freedom to make life choices",
                      "Government corruption" = "Government corruption",
                      "Generosity" = "Generosity: ")
  legendTitle <- switch(input$variableSelectorId,
                        "Happiness Score" = "Happiness Score",
                        "Economy (GDP per capita)" = "Economy",
                        "Social Support" = "Social Support",
                        "Health" = "Health",
                        "Freedom" = "Freedom",
                        "Government corruption" = "Government corruption",
                        "Generosity" = "Generosity")
  
  plot_map(data, columnDataTarget, input$color1, legendTitle, plotTitle)
})
  
  # Tab 2
  myData <- reactive({
    selectData(input$region2, input$Year2, input$country2, input$color2)
  })
    
  observe({
    data <- myData() #selectData(input$region2, input$Year2, input$country2, input$color2)
    updateSelectInput(session, "country2",
                      choices = c("None", sort(unique(data$Country))),
                      selected = input$country2
    )
  })
  
  output$plot2 <- renderPlotly(plot_relationships(input$region2, input$Year2, input$country2,
                                                  input$color2, input$Opac2))
  # Tab 3
  observe({
    x <- input$region3
    data <- selectData(input$region3, c(2015, 2019), input$country3, "Reds")
    updateSelectInput(session, "country3",
                      choices = c("None", sort(unique(data$Country))),
                      selected = input$country3
    )
  })
  output$plot3 <- renderPlotly(evolution_plot(input$region3, c(2015, 2019), input$country3, "Reds", input$jux3))
}

# Run the app
shinyApp(ui, server)

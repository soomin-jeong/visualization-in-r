library(plotly)
library(dplyr)

selectData <- function(region, yearRange, country, color){
  data <- read.csv("data/FullData.csv")
  data[is.na(data)] <- 0
  color_points = gsub("s", "", color)
  print(color_points)
  data$color = rep(color_points, nrow(data))
  
  if (country != "" & country != "None"){
    data %>% filter(Country == country & color == "black")
  }
  
  data %>% filter(Year >= yearRange[1] & Year <= yearRange[2])

  if (region == 'Europe'){
    data %>% filter(Region %in% c("Western Europe", "Central and Eastern Europe"))
  }
  else if (region == 'America'){
    data %>% filter(Region %in% c("North America","Latin America and Caribbean"))
  }
  else if (region == 'Asia'){
    data %>% filter(Region %in% c("Southeastern Asia", "Eastern Asia", "Southern Asia"))
  }
  else if (region == 'Africa & Middle East'){
    data %>% filter(Region %in% c("Sub-Saharan Africa", "Middle East and Northern Africa"))
  }
  else if (region == 'Oceanico'){
    data %>% filter(Region %in% c("Australia and New Zealand"))
  }
  return(data)
}
  

plot_relationships <- function(region, years, country, color){
  data <- selectData(region, years, country, color)
  vars <- colnames(data)[c(6, 7, 8, 9, 10, 11)]
  
  # heatmap
  cor_list <- cor(data$HappinessScore, data[vars])
  heatmap <- plot_ly(z = cor_list, x = vars, type='heatmap', colors = color)
  heatmap <- heatmap %>% layout(xaxis=list(side='top'), 
                                yaxis=list(showticklabels=FALSE,
                                           showline=FALSE,
                                           showgrid=FALSE,
                                           zeroline=FALSE))
  # Scatter plot
  marker_format = list(
    color = data$color,
    #colorscale = color,
    size = 7,
    line = list(
      width = 1,
      color = 'rgb(230,230,230)'
    )
  )
  text_format = ~paste("Country: ", Country, 
                       "\nYear: ", Year)
  
  fig1 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Economy, #color=~Region
                  marker=marker_format, mode = "markers", 
                  # Hover text:
                  text = text_format, showlegend=F)
  fig1 <- fig1 %>% layout(yaxis=list(range = c(0,10)))
  fig2 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Family, #color=~Region
                  marker=marker_format, mode = "markers",
                  # Hover text:
                  text = text_format, showlegend=F)
  fig2 <- fig2 %>% layout(yaxis=list(range = c(0,10)))
  fig3 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Health, #color=~Region
                  marker=marker_format, mode = "markers",
                  # Hover text:
                  text = text_format, showlegend=F)
  fig3 <- fig3 %>% layout(yaxis=list(range = c(0,10)))
  fig4 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Freedom, #color=~Region
                  marker=marker_format, mode = "markers",
                  # Hover text:
                  text = text_format, showlegend=F)
  fig4 <- fig4 %>% layout(yaxis=list(range = c(0,10)))
  fig5 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~GovernmentCorruption, #color=~Region
                  marker=marker_format, mode = "markers",
                  # Hover text:
                  text = text_format, showlegend=F)
  fig5 <- fig5 %>% layout(yaxis=list(range = c(0,10)))
  fig6 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Generosity, #color=~Region
                  marker=marker_format, mode = "markers",
                  # Hover text:
                  text = text_format, showlegend=F)
  fig6 <- fig6 %>% layout(yaxis=list(range = c(0,10)))
  
  scatter_fig <-subplot(fig1, fig2, fig3, fig4, fig5, fig6)
  scatter_fig <- scatter_fig %>% layout(showlegend=FALSE)
  #scatter_fig
  
  fig <- subplot(heatmap, scatter_fig, nrows=2)
  fig <- fig %>% layout(showlegend2=TRUE, legend2=list(x=10, y=10))
  return(fig)
}

# Scatter matrix

scatter_matrix <- function(region){
  data <- selectData(region)
  vars <- colnames(data)[c(6, 7, 8, 9, 10, 11)]
  df <- data[c('Country', 'Region', "Happiness.Score", vars)]
  colnames(df) <- c('Country', "Region", "Happiness", 'Economy', 'Family', 'Health', 'Freedom', 'Trust', 'Generosity')
  axis = list(showline=FALSE,
              zeroline=FALSE,
              gridcolor='#ffff',
              ticklen=7)

  fig <- df %>% plot_ly()
  fig <- fig %>%
    add_trace(
      type = 'splom',
      dimensions = list(
        list(label='Happiness', values=~Happiness),
        list(label='Economy', values=~Economy),
        list(label='Family', values=~Family),
        list(label='Health', values=~Health),
        list(label='Freedom', values=~Freedom),
        list(label='Trust', values=~Trust),
        list(label='Generosity', values=~Generosity)
      ),
      text=~Country
    )

  fig <- fig %>%
    layout(
      title= 'Happiness Dataset',
      hovermode='closest',
      dragmode= 'select',
      plot_bgcolor='rgba(240,240,240, 0.95)',
      xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=7),
      yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=7),
      xaxis2=axis,
      xaxis3=axis,
      xaxis4=axis,
      xaxis5=axis,
      xaxis6=axis,
      xaxis7=axis

    )

  fig
}

line_chart <- function(region, years, country, color){
  data <- selectData(region, years, country, color)
  
  mean_data <- aggregate(data$HappinessScore, list(data$Year), mean)
  colnames(mean_data) <- c('Year', "HappinessScore")
  
  max_data <- aggregate(data$HappinessScore, list(data$Year), max)
  colnames(max_data) <- c('Year', "HappinessScore")
  
  min_data <- aggregate(data$HappinessScore, list(data$Year), min)
  colnames(min_data) <- c('Year', "HappinessScore")
  
  fig <- plot_ly(mean_data, x=~Year, y=~HappinessScore, type='scatter', mode='lines',
                 name= "Average")
  fig <- fig %>% add_trace(data=max_data, y=~HappinessScore, name="Max")
  fig <- fig %>% add_trace(data=min_data, y=~HappinessScore, name= "Min")
  
  if (country != "" & country != "None"){
    country_data = data[data$Country == country, ]
    fig <- fig %>% add_trace(data=country_data, y=~HappinessScore, name= country)
  }
  
  fig <- fig %>% layout(yaxis=list(range = c(0,10)))
  fig
}


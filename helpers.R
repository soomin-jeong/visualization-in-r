library(plotly)

# Function to help with data filtering and coloring
selectData <- function(region, yearRange, country, color){
  data <- read.csv("FullData.csv")
  data[is.na(data)] <- 0
  color_points = gsub("s", "", color)
  data$color = rep(color_points, nrow(data))
  if (country != "" & country != "None"){
    data[data$Country == country, ]$color = "black"
  }
  if (length(yearRange) != 1){
    data <- data[(data$Year >= yearRange[1]) & (data$Year <= yearRange[2]), ]
  }
  else{
    data <- data[(data$Year == yearRange[1]), ]
  }
  if (region == 'All'){
    return(data)
  }
  else if (region == 'Europe'){
    data <- data[(data$Region == "Western Europe") | 
                   (data$Region ==  "Central and Eastern Europe"), ] 
  }
  else if (region == 'America'){
    data <- data[(data$Region == "North America") | 
                   (data$Region ==  "Latin America and Caribbean"), ] 
  }
  else if (region == 'Asia'){
    data <- data[(data$Region == "Southeastern Asia") | 
                   (data$Region ==   "Eastern Asia" |
                      (data$Region == "Southern Asia")), ] 
  }
  else if (region == 'Africa & Middle East'){
    data <- data[(data$Region == "Sub-Saharan Africa") | 
                   (data$Region ==   "Middle East and Northern Africa"), ]
  }
  else{
    data <- data[(data$Region == "Australia and New Zealand"),]   
  }
  
  return(data)
}

# Spatial plot
plot_map <- function (dataTarget, dataTargetColumn, color, legendTitle, plotTitle) {
  l <- list(color = toRGB("grey"), width = 0.5)
  
  fig <- plot_geo(dataTarget)
  fig <- fig %>% add_trace(
    z = dataTargetColumn, color = dataTargetColumn, colors = color,
    text = dataTarget$Country, locations = dataTarget$countriesCodes, marker = list(line = l)
  )
  fig <- fig %>% colorbar(title = legendTitle)
  fig <- fig %>% layout(
    title = plotTitle,
    autosize = T
  )
  
  fig
}


plot_relationships <- function(region, years, country, color, opac){
  data <- selectData(region, years, country, color)
  names(data)[names(data) == 'Family'] <- 'SocialSupport'
  vars <- colnames(data)[c(6, 7, 8, 9, 10, 11)]
  
  # Heatmap
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
    size = 7,
    opacity = opac,
    line = list(
      width = 1,
      color = 'rgb(230,230,230)')
  )
  text_format = ~paste("Country: ", Country, 
                       "\nYear: ", Year)
  # if (country != "" & country != "None"){
  #   country_data <- data[data$Country == country, ]
  #   print(country_data)
  #   data <- data[data$Country != country, ]
  # }
  
  fig1 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Economy, #color=~Region
                  marker=marker_format, mode = "markers", text = text_format, showlegend=F)
  fig1 <- fig1 %>% layout(yaxis=list(range = c(0,10)))
  
  fig2 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~SocialSupport, #color=~Region
                  marker=marker_format, mode = "markers",text = text_format, showlegend=F)
  fig2 <- fig2 %>% layout(yaxis=list(range = c(0,10)))
  
  fig3 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Health, #color=~Region
                  marker=marker_format, mode = "markers", text = text_format, showlegend=F)
  fig3 <- fig3 %>% layout(yaxis=list(range = c(0,10)))
  
  fig4 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Freedom, #color=~Region
                  marker=marker_format, mode = "markers", text = text_format, showlegend=F)
  fig4 <- fig4 %>% layout(yaxis=list(range = c(0,10)))
  
  fig5 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~GovernmentCorruption, #color=~Region
                  marker=marker_format, mode = "markers", text = text_format, showlegend=F)
  fig5 <- fig5 %>% layout(yaxis=list(range = c(0,10)))
  
  fig6 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Generosity, #color=~Region
                  marker=marker_format, mode = "markers", text = text_format, showlegend=F)
  fig6 <- fig6 %>% layout(yaxis=list(range = c(0,10)))
  
  # if (country != "" & country != "None"){
  #   fig1 <- fig1 %>% add_trace(data=country_data, x=~Economy, 
  #                              y=~HappinessScore, marker=marker_format)
  # }
  
  scatter_fig <-subplot(fig1, fig2, fig3, fig4, fig5, fig6)
  scatter_fig <- scatter_fig %>% layout(showlegend=FALSE)
  
  fig <- subplot(heatmap, scatter_fig, nrows=2)
  fig <- fig %>% layout(showlegend2=TRUE, legend2=list(x=10, y=10))
  return(fig)
}


line_chart <- function(data, country, region){
  mean_data <- aggregate(data$HappinessScore, list(data$Year), mean)
  colnames(mean_data) <- c('Year', "HappinessScore")
  
  max_data <- aggregate(data$HappinessScore, list(data$Year), max)
  colnames(max_data) <- c('Year', "HappinessScore")
  
  min_data <- aggregate(data$HappinessScore, list(data$Year), min)
  colnames(min_data) <- c('Year', "HappinessScore")
  
  fig <- plot_ly(mean_data, x=~Year, y=~HappinessScore, type='scatter', mode='lines',
                 name= paste("Average", region, sep="_"), line=list(color="Blue"))
  fig <- fig %>% add_trace(data=max_data, y=~HappinessScore, 
                           name=paste("Max", region, sep="_"), line=list(color="Orange"))
  fig <- fig %>% add_trace(data=min_data, y=~HappinessScore, name= paste("Min", region, sep="_")
                           , line=list(color="Green"))
  
  if (country != "" & country != "None"){
    country_data = data[data$Country == country, ]
    fig <- fig %>% add_trace(data=country_data, y=~HappinessScore, name= country, line=list(color="Red"))
  }
  
  fig <- fig %>% layout(yaxis=list(range = c(0,10)), 
                        xaxis=list(dtick=1), 
    annotations = list(x=2017, y =10, text=region, showarrow=F, 
                       xref='x', yref='y', ax= 20, ay = 20)
  )
  return(fig)
}


# Evolution Plot
evolution_plot <- function(region, years, country, color, jux){
  if (jux == FALSE){
    data <- selectData(region, years, country, color)
    fig <- line_chart(data, country, region)
  }
  else{
    
    reg = "All"
    data <- selectData(reg, years, country, color)
    fig_all <- line_chart(data, country, reg)
    
    reg = "Africa & Middle East"
    data <- selectData(reg, years, country, color)
    fig1 <- line_chart(data, country, reg)
    
    reg = "America"
    data <- selectData(reg, years, country, color)
    fig2 <- line_chart(data, country, reg)
    
    reg = "Asia"  
    data <- selectData(reg, years, country, color)
    fig3 <- line_chart(data, country, reg)
    
    reg = "Europe"
    data <- selectData(reg, years, country, color)
    fig4 <- line_chart(data, country, reg)
    
    reg = "Oceania"
    data <- selectData(reg, years, country, color)
    fig5 <- line_chart(data, country, reg)
    
    fig <- subplot(fig_all, fig1, fig2, fig3, fig4, fig5, nrows=1)
  
    return(fig)
  }
}



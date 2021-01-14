library(plotly)


selectData <- function(region, yearRange, country){
  data <- read.csv("FullData.csv")
  data[is.na(data)] <- 0
  data$color = rep('rgb(51, 153, 255)', nrow(data))
  if (country != ""){
    data[data$Country == country, ]$color = "orange"
  }
  data <- data[(data$Year >= yearRange[1]) & (data$Year <= yearRange[2]), ]
  if (region == 'All'){
    return(data)
  }
  else{
    data <- data[data$Region == region, ]    
  }
  
  return(data)
}
  

plot_relationships <- function(region, years, country){
  print(years)
  data <- selectData(region, years, country)
  vars <- colnames(data)[c(6, 7, 8, 9, 10, 11)]
  print(vars)
  
  # heatmap
  cor_list <- cor(data$HappinessScore, data[vars])
  heatmap <- plot_ly(z = cor_list, x = vars, type='heatmap')
  heatmap <- heatmap %>% layout(xaxis=list(side='top'), yaxis=list(showticklabels=FALSE, 
                                                                   showline=FALSE,
                                                                   showgrid=FALSE,
                                                                   zeroline=FALSE))
  
  # Scatter plot
  marker_format = list(
    color = data$color,
    #colorscale = pl_colorscale,
    size = 7,
    line = list(
      width = 1,
      color = 'rgb(230,230,230)'
    )
  )
  text_format = ~paste("Country: ", Country, 
                       "\nYear: ", Year)
  
  fig1 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Economy, #color=~Region
                  marker=marker_format,
                  # Hover text:
                  text = text_format, showlegend=F)
  fig2 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Family, #color=~Region
                  marker=marker_format,
                  # Hover text:
                  text = text_format, showlegend=F)
  fig3 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Health, #color=~Region
                  marker=marker_format,
                  # Hover text:
                  text = text_format, showlegend=F)
  fig4 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Freedom, #color=~Region
                  marker=marker_format,
                  # Hover text:
                  text = text_format, showlegend=F)
  fig5 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~GovernmentCorruption, #color=~Region
                  marker=marker_format,
                  # Hover text:
                  text = text_format, showlegend=F)
  fig6 <- plot_ly(type= 'scatter', data = data, y=~HappinessScore, x=~Generosity, #color=~Region
                  marker=marker_format,
                  # Hover text:
                  text = text_format, showlegend=F)
  
  scatter_fig <-subplot(fig1, fig2, fig3, fig4, fig5, fig6)
  scatter_fig <- scatter_fig %>% layout(showlegend=FALSE)
  #scatter_fig
  
  fig <- subplot(heatmap, scatter_fig, nrows=2)
  fig <- fig %>% layout(showlegend2=TRUE, legend2=list(x=10, y=10))
  return(fig)
}

# 
# # Scatter matrix

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
      # marker = list(
      #   #color = as.integer(df),
      #   #colorscale = pl_colorscale,
      #   size = 7,
      #   line = list(
      #     width = 1,
      #     color = 'rgb(230,230,230)'
      #   )
      # )
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

# 

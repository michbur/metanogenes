#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    dat <- read.csv("visual_data.csv")
    

    gg_plot <- ggplot(dat, aes_string(x = input[["VarX"]], 
                           y = input[["VarY"]], 
                           color = input[["VarCol"]],
                           text = "Name")) +
      geom_point() + 
      theme_bw() #+ theme(legend.direction = "horizontal", legend.position = "bottom")
    
    gg_plotly <- plotly_build(gg_plot)
    #gg_plotly[["x"]][["layout"]][["legend"]] <- c(gg_plotly[["x"]][["layout"]][["legend"]], orientation = "h")
    gg_plotly %>%  layout(#xaxis = list(showticklabels = FALSE),
                          legend = list(orientation = "h",
                                        y = 0, x = 0))
  })
  
})


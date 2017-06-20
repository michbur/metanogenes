#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)

dat <- read.csv("visual_data.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel("MetPhy2 Feature Visualisation"),
  
  
  
  # Sidebar with a slider input for number of bins 
  verticalLayout(
    fluidRow(
      column(3, selectInput("VarX", "Variable (X-axis):",
                            colnames(dat)[-1], selected = colnames(dat)[-1][1])),
      column(3, selectInput("VarY", "Variable (Y-axis):",
                            colnames(dat)[-1], selected = colnames(dat)[-1][5])),
      column(3, selectInput("VarCol", "Variable (color):",
                            colnames(dat)[-1], selected = colnames(dat)[-1][3]))
    ),
    
    # Show a plot of the generated distribution
      plotlyOutput("distPlot", height = "500px")
  )
))

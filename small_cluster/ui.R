library(shiny)
library(plotly)
library(rhandsontable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Metanogenes clusterization"),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    rHandsontableOutput("input_table"),
    plotlyOutput("clPlot", height = "1600px", width = "1600px")
  )
)
)

library(ggplot2)
library(ggdendro)
library(dplyr)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output[["clPlot"]] <- renderPlot({
    
    dat <- read.csv("metanogenes_partial.csv") 
    
    rownames(dat) <- dat[["Name"]]
    cl_dat <- dat[-1]
    
    model <- cl_dat %>% dist %>% hclust 
    
    ddata <- dendro_data(model, type = "triangle")
    
    ggplot(segment(ddata)) + 
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
      scale_x_continuous("", breaks = label(ddata)[["x"]], 
                         labels = label(ddata)[["label"]]) +
      coord_flip() +
      theme_bw() +
      theme(panel.grid.major = element_line(color=NA),
            panel.grid.minor = element_line(color=NA),
            axis.text.y = element_text(size = 12)) +
      scale_y_continuous("Distance")
    
  }, height = 1600)
  
})

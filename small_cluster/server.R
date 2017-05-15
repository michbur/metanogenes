library(ggplot2)
library(ggdendro)
library(dplyr)
library(shiny)
library(plotly)
library(cluster)
library(rhandsontable)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  final_dat <- reactive({
    
    dat <- if(is.null(input[["input_table"]])) {
      raw_dat <- read.csv("metanogenes_partial.csv") 
      na.omit(raw_dat[which(rowSums(apply(raw_dat, 1, is.na)) < 10)])
    } else {
      hot_to_r(input[["input_table"]])
    }

    dat
  })
  
  filter_final_dat <- reactive({
    dat <- final_dat()
    
    empty_names <- is.na(dat[["Name"]]) | (dat[["Name"]] == "NA")
    if(sum(empty_names) > 0) {
      dat <- dat[!empty_names, ]
    }
    
    rownames(dat) <- dat[["Name"]]
    dat[-1]
  })
  
  
  output[["input_table"]] = renderRHandsontable({
    dat <- final_dat()
    #dat <- cbind(Name = rownames(dat), dat)
    #rownames(dat) <- NULL
    rhandsontable(dat, useTypes = FALSE, 
                  readOnly = FALSE, selectCallback = TRUE,
                  highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>% 
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  
  output[["clPlot"]] <- renderPlotly({
    
    
    x <- scale(filter_final_dat())
    
    dd.col <- try(as.dendrogram(hclust(daisy(x, metric="gower"))))
    dd.row <- as.dendrogram(hclust(daisy(t(x), metric="gower")))
    
    if(class(dd.col) == "try-error")
      browser()
    dx <- dendro_data(dd.row)
    dy <- dendro_data(dd.col)
    
    # helper function for creating dendograms
    ggdend <- function(df) {
      ggplot() +
        geom_segment(data = df, aes(x=x, y=y, xend=xend, yend=yend)) +
        labs(x = "", y = "") + theme_minimal() +
        theme(axis.text = element_blank(), axis.ticks = element_blank(),
              panel.grid = element_blank())
    }
    
    # x/y dendograms
    px <- ggdend(dx$segments)
    py <- ggdend(dy$segments) + coord_flip()
    
    # heatmap
    col.ord <- order.dendrogram(dd.col)
    row.ord <- order.dendrogram(dd.row)
    xx <- scale(filter_final_dat())[col.ord, row.ord]
    xx_names <- attr(xx, "dimnames")
    df <- as.data.frame(xx)
    colnames(df) <- xx_names[[2]]
    df$Name <- xx_names[[1]]
    df$Name <- with(df, factor(Name, levels=Name, ordered=TRUE))
    mdf <- reshape2::melt(df, id.vars="Name")
    p <- ggplot(mdf, aes(x = variable, y = Name)) + 
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = "blue", high = "red") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    # hide axis ticks and grid lines
    eaxis <- list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    )
    
    p_empty <- plot_ly() %>%
      # note that margin applies to entire plot, so we can
      # add it here to make tick labels more readable
      layout(margin = list(l = -200),
             xaxis = eaxis,
             yaxis = eaxis)
    
    subplot(px, p, nrows = 2)
    
  })
  
})

library(ggplot2)
library(plotly)
library(dplyr)
library(pbapply)

dat <- read.csv("./featureVisualisation/visual_data.csv")

VarX <- colnames(dat)[-1][1]
VarY <- colnames(dat)[-1][8]
VarCol <- colnames(dat)[-1][3]

color_palettes <- list(A = scale_color_continuous(high = "blue", low = "#FF0000", na.value = "black"),
                       B = scale_color_continuous(low = "#d17312", high = "#04d1b2", na.value = "black"))


lapply(c("A", "B"), function(single_palette) 
  pblapply(colnames(dat)[-1], function(VarX) 
    lapply(colnames(dat)[-1], function(VarY) 
      lapply(colnames(dat)[-1], function(VarCol) {
        gg_plot <- ggplot(dat[, c("Name", VarX, VarY, VarCol)], aes_string(x = VarX, 
                                               y = VarY, 
                                               color = VarCol,
                                               text = "Name")) +
          geom_point(size = 2) + 
          theme_bw() +
          color_palettes[[single_palette]]
        
        gg_plotly <- plotly_build(gg_plot)
        
        file_name <- paste0(getwd(), "/FV/", single_palette, "/", VarX, "_", VarY, "_", VarCol, ".html")
        
        htmlwidgets::saveWidget(as_widget(gg_plotly), file_name, 
                                selfcontained = FALSE, 
                                libdir = paste0(getwd(), "/FV/", single_palette, "/plotly_files"))
        
      })
    )
  )
)

#gg_plotly[["x"]][["layout"]][["legend"]] <- c(gg_plotly[["x"]][["layout"]][["legend"]], orientation = "h")
# gg_plotly %>%  layout(#xaxis = list(showticklabels = FALSE),
#   legend = list(orientation = "h",
#                 y = 0, x = 0))
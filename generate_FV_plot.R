library(ggplot2)
library(plotly)
library(dplyr)
library(pbapply)

dat <- read.csv("./featureVisualisation/visual_data.csv")

nice_names <- readLines("full_names.txt") %>% 
  strsplit('<OPTION value=\"', fixed = TRUE) %>% 
  first %>% 
  strsplit('\" > ', fixed = TRUE) %>% 
  unlist %>% 
  strsplit('\t\t\t\t\t', fixed = TRUE) %>% 
  unlist %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  data.frame(row.names = .[, 1])

VarX <- colnames(dat)[-1][1]
VarY <- colnames(dat)[-1][18]
VarCol <- colnames(dat)[-1][3]

color_palettes <- list(A = c(high = "blue", low = "#FF0000"),
                       B = c(low = "#d17312", high = "#04d1b2"))


# lapply(c("A", "B"), function(single_palette) 
#   pblapply(colnames(dat)[-1], function(VarX) 
#     lapply(colnames(dat)[-1], function(VarY) 
#       lapply(colnames(dat)[-1], function(VarCol) {
#         pal <- color_palettes[[single_palette]]
#           
#         gg_plot <- ggplot(dat[, c("Name", VarX, VarY, VarCol)], aes_string(x = VarX, 
#                                                y = VarY, 
#                                                color = VarCol,
#                                                text = "Name")) +
#           geom_point(size = 2) + 
#           theme_bw() +
#           scale_x_continuous(as.character(nice_names[VarX, "X2"])) +
#           scale_y_continuous(as.character(nice_names[VarY, "X2"])) +
#           scale_color_continuous(as.character(nice_names[VarCol, "X2"]),
#                                  high = pal[1], low = pal[2],
#                                  na.value = "black")
#         
#         gg_plotly <- plotly_build(gg_plot)
#         
#         #attr(gg_plotly[["x"]][["data"]][[1]][["text"]], "apiSrc")
#         
#         text_dat <- strsplit(gg_plotly[["x"]][["data"]][[1]][["text"]], "<br />", fixed = TRUE) %>% 
#           do.call(rbind, .)
#           
#         gg_plotly[["x"]][["data"]][[1]][["text"]] <- matrix(c(text_dat[, 2],
#                                                               gsub(VarCol, nice_names[VarCol, "X2"], text_dat[, 1]),
#                                                               gsub(VarX, nice_names[VarX, "X2"], text_dat[, 3]),
#                                                               gsub(VarY, nice_names[VarY, "X2"], text_dat[, 4])
#         ), ncol = 4) %>% 
#           apply(1, function(i) paste0(i, collapse = "<br />"))
#         
#         file_name <- paste0(getwd(), "/FV/", single_palette, "/", VarX, "_", VarY, "_", VarCol, ".html")
# 
#         htmlwidgets::saveWidget(as_widget(gg_plotly), file_name, 
#                                 selfcontained = FALSE, 
#                                 libdir = paste0(getwd(), "/FV/", single_palette, "/plotly_files"))
#         
#       })
#     )
#   )
# )

lapply(c("A", "B"), function(single_palette) 
  pblapply(colnames(dat)[-1], function(VarX) 
    lapply(colnames(dat)[-1], function(VarY) 
      lapply(colnames(dat)[-1], function(VarCol) {
        pal <- color_palettes[[single_palette]]
        
        gg_plot <- ggplot(dat[, c("Name", VarX, VarY, VarCol)], aes_string(x = VarX, 
                                                                           y = VarY, 
                                                                           color = VarCol,
                                                                           text = "Name")) +
          geom_point(size = 2) + 
          theme_bw(base_size = 8) +
          scale_x_continuous(as.character(nice_names[VarX, "X2"])) +
          scale_y_continuous(as.character(nice_names[VarY, "X2"])) +
          scale_color_continuous(as.character(nice_names[VarCol, "X2"]),
                                 high = pal[1], low = pal[2],
                                 na.value = "black")
        
        gg_plotly <- plotly_build(gg_plot)
        
        file_name <- paste0(getwd(), "/FV_static/", single_palette, "/", VarX, "_", VarY, "_", VarCol, ".png")

        export(p = gg_plotly, file = file_name)
        
      })
    )
  )
)

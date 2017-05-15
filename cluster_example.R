library(ggplot2)
library(dplyr)
library(fpc)
library(factoextra)

dat <- read.csv("metanogenes_partial.csv")

small_dat <- na.omit(dat[which(rowSums(apply(dat, 1, is.na)) < 10)])

cairo_ps("cluster.eps", width = 8, height = 8, pointsize = 1)
fviz_cluster(dbscan(small_dat[, -1], eps = 5, MinPts = 4), small_dat[, -1], geom = "point") +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

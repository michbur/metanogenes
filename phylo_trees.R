library(ggplot2)
library(plotly)
library(dplyr)
library(pbapply)
library(ggtree)

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

load("mcrA_tree.RData")




fort_tree2 <- left_join(fort_tree, select(dat, Name, optimalGrowthPHMinimal), by = c("label" = "Name"))


svg("tmp_name.svg", width = 18, height = 40, pointsize = 12)
ggtree(fort_tree2, branch.length = "branch.length", aes(color = optimalGrowthPHMinimal)) +
  geom_tiplab(size = 7) +
  geom_tippoint(size = 2) +
  geom_label(aes(label=bootstrap), size = 3.5, hjust = -0.05, label.size = NA, fill = NA) +
  geom_treescale() +
  ggplot2:::limits(c(0, 2.5), "x") +
  scale_color_continuous(low = "#d17312", high = "#04d1b2") +
  theme(legend.position = "bottom") 
dev.off()

# http://veg.github.io/phylotree.js/index.html

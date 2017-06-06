library(rentrez)
library(seqinr)
library(dplyr)
library(XML)

raw_dat <- readLines("export_data_all_110117.csv") %>% 
  strsplit(split = "| ", fixed = TRUE) %>% 
  do.call(rbind, .)

dat <- raw_dat[-1, ]
colnames(dat) <- raw_dat[1, ]

taxonomies <- lapply(dat[, "Name"], function(single_name) {
  tax_ids <- entrez_search(db = "taxonomy", term = paste0(single_name, "[ORGN]"))
  
  entrez_fetch(db = "taxonomy", id = tax_ids[["ids"]], rettype = "text") %>%  
    xmlToDataFrame() %>% 
    select(Lineage) %>% 
    unlist %>% 
    as.character()
})

data.frame(Name = names(taxonomies), taxonomy = sapply(taxonomies, first)) %>% 
  mutate(taxonomy = as.character(taxonomy)) %>% 
  mutate(taxonomy = sapply(taxonomy, function(i) {
    strsplit(i, "cellular organisms; ")[[1]][[2]]
  })) %>% 
  write.csv("taxonomies.csv", row.names = FALSE)

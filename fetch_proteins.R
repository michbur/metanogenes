library(rentrez)
library(seqinr)
library(dplyr)


raw_dat <- readLines("export_data_all_110117.csv") %>% 
  strsplit(split = "| ", fixed = TRUE) %>% 
  do.call(rbind, .)

dat <- raw_dat[-1, ]
colnames(dat) <- raw_dat[1, ]


# for(single_name in dat[, "Name"]) {
for(single_name in dat[, "Name"]) {
  gene_ids <- entrez_search(db = "protein", term = paste0(single_name, "[ORGN]"))
  
  genes <- entrez_fetch(db = "protein", id = gene_ids[["ids"]], rettype = "fasta")
  cat(genes, file = paste0("proteins/", single_name, ".fasta"))
}

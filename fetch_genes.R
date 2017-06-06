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
  gene_ids <- entrez_search(db = "nucleotide", term = paste0(single_name, "[ORGN]"))
  
  genes <- entrez_fetch(db = "nucleotide", id = gene_ids[["ids"]], rettype = "fasta")
  cat(genes, file = paste0("genes/", single_name, ".fasta"))
}

all_files <- list.files("./genes/")

all_seqs <- do.call(rbind, lapply(all_files, function(i)
  data.frame(organism = strsplit(i, ".fasta", fixed = TRUE)[[1]],
             seq = names(read.fasta(paste0("./genes/", i))))
))


mutate(all_seqs, address = paste0("https://www.ncbi.nlm.nih.gov/nuccore/", seq)) %>% 
  write.csv2(row.names = FALSE, file = "all_seqs.csv")

# gb genes

for(single_name in dat[, "Name"]) {
  gene_ids <- entrez_search(db = "nucleotide", term = paste0(single_name, "[ORGN]"))
  
  genes <- entrez_fetch(db = "nucleotide", id = gene_ids[["ids"]], rettype = "gb")
  cat(genes, file = paste0("genes_gb/", single_name, ".gb"))
}

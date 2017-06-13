library(seqinr)
library(dplyr)
library(pbapply)

single_file <- list.files("genes")[1]

genome_stuff <- pblapply(list.files("genes"), function(single_file) {
  tmp <- read.fasta(paste0("genes/", single_file), seqtype = "AA") %>% 
    sapply(function(i) attr(i, "Annot"))
  
  tmp[grep(tmp, pattern = "complete genome")]
})

unlist(genome_stuff) %>% 
  unname %>% 
  (function(x) regmatches(x, regexpr(" ", text = x), invert = TRUE)) %>% 
  sapply(last) %>% 
  unique %>% 
  strsplit(", ") %>% 
  sapply(first) %>% 
  cat(sep = "\n")

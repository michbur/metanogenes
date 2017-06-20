library(dplyr)
library(biogram)
library(pbapply)

raw_dat <- readLines("export_data_all_110117.csv") %>% 
  strsplit(split = "| ", fixed = TRUE) %>% 
  do.call(rbind, .)

dat <- raw_dat[-1, ]
colnames(dat) <- raw_dat[1, ]
num_ids <- sapply(1L:ncol(dat), function(i) {
  single_col <- dat[, i]
  only_sure <- single_col[single_col != "no data"]
  !any(is.na(as.numeric(only_sure)))
})

numeric_dat <- sub(pattern = "no data", replacement = NA, dat[, num_ids])
storage.mode(numeric_dat) <- "numeric"

final_dat <- data.frame(Name = dat[, "Name"], numeric_dat, dat[, -num_ids])

all_species <- list.files("proteins/")

single_file <- all_species[1]

res <- pblapply(all_species, function(single_file)
  try(seqinr::read.fasta(paste0("./proteins/", single_file), seqtype = "AA") %>% 
    unlist %>% list(x = .) %>% 
    list2matrix() %>% count_ngrams(u = seqinr::a()[-1], n = 2), silent = TRUE)
)

pca_res <- res[sapply(res, class) != "try-error"] %>% 
  do.call(rbind, .) %>% 
  as.matrix %>% 
  prcomp(center = TRUE, scale = TRUE) %>%
  getElement("x") %>% 
  data.frame() %>% 
  select(PC1, PC2) %>% 
  mutate(Name = unlist(strsplit(all_species[sapply(res, class) != "try-error"], ".fasta"))) %>% 
  inner_join(final_dat)

write.csv(pca_res, file = "./ngram_app/metanogenes_partial.csv", row.names = FALSE)

library(ggplot2)
ggplot(pca_res, aes(x = PC1, y = PC2, color = ethanol)) +
  geom_point()

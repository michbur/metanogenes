library(dplyr)

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

final_dat <- data.frame(Name = dat[, "Name"], numeric_dat)

write.csv(final_dat, file = "metanogenes_partial.csv", row.names = FALSE)

library(dplyr)

raw_dat <- readLines("export_data_all_110117.csv") %>% 
  strsplit(split = "| ", fixed = TRUE) %>% 
  do.call(rbind, .)

dat <- raw_dat[-1, ]
colnames(dat) <- raw_dat[1, ]

num_ids <- sapply(1L:ncol(dat), function(i) {
  single_col <- dat[, i]
  only_sure <- single_col[single_col != "no data"]
  sum(is.na(as.numeric(only_sure)))/length(only_sure)
})

dat_df <- data.frame(dat)
for(i in which(num_ids == 0)) {
  dat_df[[i]] <- as.numeric(dat_df[[i]])
}


# levels(dat_df[["AdditionalGrowthRequierments"]]) %>%
#   tolower %>%
#   strsplit(", *") %>% unlist %>%
#   strsplit(" and ") %>% unlist %>%
#   strsplit("; ") %>% unlist %>%
#   strsplit("\\+") %>% unlist %>%
#   unique %>% 
#   sort %>%
#   data.frame(old_name = .) %>%
#   write.csv(file = "AdditionalGrowthRequierments.csv", row.names = FALSE)

# levels(dat_df[["MinimumGrowthRequierments"]]) %>% 
#   tolower %>% 
#   strsplit(", *") %>% unlist %>% 
#   strsplit(", *") %>% unlist %>%
#   strsplit(" and ") %>% unlist %>%
#   strsplit(" or ") %>% unlist %>%
#   strsplit("; ") %>% unlist %>%
#   strsplit("\\+") %>% unlist %>%
#   strsplit("\\\\") %>% unlist %>%
#   strsplit("/") %>% unlist %>%
#   unique %>%
#   sort %>% 
#   data.frame(old_name = .) %>%
#   write.csv(file = "MinimumGrowthRequierments.csv", row.names = FALSE)

levels(dat_df[["MinimumGrowthRequierments"]])[grep("branched", levels(dat_df[["MinimumGrowthRequierments"]]))]

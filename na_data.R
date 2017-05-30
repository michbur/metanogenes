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

# stringsAsFactors = FALSE) %>% 
#   mutate(changed = old_name != new_name) %>% 
#   print(head = FALSE)

data.frame(old_name = levels(dat_df[["cellShape"]]),
           new_name = c("Cocci", "Coccobacillus", "Irregular cocci",
                        "Irregular disc", "Bacillus", "Coccobacillus",
                        "Sheathed spiral")) %>% 
  write.csv(file = "./changed_cols/cellShape.csv", row.names = FALSE) 


data.frame(old_name = levels(dat_df[["creator"]]),
           new_name = c("negative", "no data", "no data", "positive", "positive", 
                        "unknown (cell lysis)", "variable"))  %>% 
  write.csv(file = "./changed_cols/creator.csv", row.names = FALSE) 

data.frame(old_name = levels(dat_df[["gramReaction"]]),
           new_name = c("negative", "no data", "no data", "positive", "positive", 
                        "unknown (cell lysis)", "variable")) %>% 
  write.csv(file = "./changed_cols/gramReaction.csv", row.names = FALSE) 

data.frame(old_name = levels(dat_df[["maximumcellGcContent"]]),
           new_name = c("no data", "no data", "no data", "no data", 
                        "present", "no data")) %>% 
  write.csv(file = "./changed_cols/maximumcellGcContent.csv", row.names = FALSE) 

# Requirements ------------------------------------

# uniformize names - compounds from symbols to full names (B12)

levels(dat_df[["MinimumGrowthRequierments"]]) %>%
  strsplit(", *") %>% unlist %>%
  strsplit(", *") %>% unlist %>%
  strsplit(" and ") %>% unlist %>%
  strsplit(" or ") %>% unlist %>%
  strsplit("; ") %>% unlist %>%
  strsplit("\\+") %>% unlist %>%
  strsplit("\\\\") %>% unlist %>%
  strsplit("/") %>% unlist %>%
  unique %>%
  sort %>%
  data.frame(old_name = ., stringsAsFactors = FALSE) %>% 
  mutate(new_name = c("???", "2-methylbutyrate", "acetate", "acetate", "aminoacids", 
                      "aminobenzoate", "B12", "biotin", "branched-chain", "Ca", "casamino acids", 
                      "casamino acids", "casitone", "Co", "CO2", "coenzyme M", "coenzyme M", 
                      "coenzyme M", "ethanol", "Fe", "H2", "heavy metal solution", "isoleucine", 
                      "leucine", "MB medium", "methanol", "Mg", "Na2S", "NaCl", "Ni", 
                      "none", "no data", "nutrient broth", "vitamins", 
                      "peptone", "peptone", "propionate", "riboflavin", "rumen fluid", 
                      "rumen fluid", "rumen fuid", "SeO4", "thiamine", "thiamine", 
                      "trace element solution", 
                      "trypticase", "trypticase", "trypticase peptone", "trypticase", 
                      "tryptone", "tungstate", "tungsten", "no data", "vitamins", 
                      "vitamins", "B12", "yeast extract", "yeast extract")) %>% 
  mutate(original_record = sapply(old_name, function(ith_name) 
    as.character(dat_df[["MinimumGrowthRequierments"]][grep(ith_name, as.character(dat_df[["MinimumGrowthRequierments"]]))])[1]),
    Name = sapply(old_name, function(ith_name) 
      as.character(dat_df[["Name"]][grep(ith_name, as.character(dat_df[["MinimumGrowthRequierments"]]))])[1])
  ) %>% write.csv(file = "./changed_cols/MinimumGrowthRequierments.csv", row.names = FALSE) 


levels(dat_df[["AdditionalGrowthRequierments"]]) %>%
  strsplit(", *") %>% unlist %>%
  strsplit(", *") %>% unlist %>%
  strsplit(" and ") %>% unlist %>%
  strsplit(" or ") %>% unlist %>%
  strsplit("; ") %>% unlist %>%
  strsplit("\\+") %>% unlist %>%
  strsplit("\\\\") %>% unlist %>%
  strsplit("/") %>% unlist %>%
  unique %>%
  sort %>%
  data.frame(old_name = ., stringsAsFactors = FALSE) %>% 
  mutate(new_name = c("???", "acetate", "acetate", "acetate", "acetate", "aminoacids", 
                      "trimethylamine", "biotin", "C02", "casamino acids", "CO", 
                      "CoM", "cysteine", "dimethylamine", "yeast extract", "Fe", "Fe2O3", 
                      "generally organic compounds", 
                      "H2", "HS-coenzyme M", "methanethiol", "methanol", "methylamine", 
                      "microelement solution", 
                      "monomethylamine", "Ni", "none", "none", "none", "no data", "pantolactone", 
                      "peptone", "peptone ", "K", "rumen fluid", "rumen fluid", 
                      "selenate", "selenite", "Se", "Se", "trypticase", 
                      "trypticase", "tungsten", "vitamins", "tryptophan", "yeast extract", "yeast extract", 
                      "yeast extract")) %>% 
  mutate(original_record = sapply(old_name, function(ith_name) 
    as.character(dat_df[["AdditionalGrowthRequierments"]][grep(ith_name, as.character(dat_df[["AdditionalGrowthRequierments"]]))])[1]),
    Name = sapply(old_name, function(ith_name) 
      as.character(dat_df[["Name"]][grep(ith_name, as.character(dat_df[["AdditionalGrowthRequierments"]]))])[1])
  ) %>% write.csv(file = "./changed_cols/AdditionalGrowthRequierments.csv", row.names = FALSE) 


levels(dat_df[["otherSubstrate"]]) %>%
  strsplit(", ") %>% unlist %>%
  strsplit(" and ") %>% unlist %>%
  strsplit(" or ") %>% unlist %>%
  strsplit("; ") %>% unlist %>%
  strsplit("\\+") %>% unlist %>%
  strsplit("\\\\") %>% unlist %>%
  strsplit("/") %>% unlist %>%
  gsub("not supporting[ ]?:[ ]?", "", x = .) %>% 
  gsub("not suppoting:", "", x = .) %>% 
  unique %>%
  sort %>%
  data.frame(old_name = ., stringsAsFactors = FALSE) %>% 
  mutate(new_name = c("1-pentanol", "2-pentanol", "3-butanodiol", "alanine", 
                      "butyrate", "cyclohexanol", "cyclohexanol", "ethyl acetate", 
                      "formaldehyde", "fructose", "fumarate", "glucose", "H2", "lactate", 
                      "leucine", "methanetiol", "methanol", "no data", "pyruvate", 
                      "theobromine", "theophyline", "trimethoxybenzoate", "valerate")) %>% 
  mutate(original_record = sapply(old_name, function(ith_name) 
    as.character(dat_df[["otherSubstrate"]][grep(ith_name, as.character(dat_df[["otherSubstrate"]]))])[1]),
    Name = sapply(old_name, function(ith_name) 
      as.character(dat_df[["Name"]][grep(ith_name, as.character(dat_df[["otherSubstrate"]]))])[1])) %>% 
  write.csv(file = "./changed_cols/otherSubstrate.csv", row.names = FALSE) 

as.character(dat_df[["otherATTCStrainNumbers"]]) %>%
  strsplit(", *") %>% unlist %>% table %>% sort

as.character(dat_df[["otherCollections"]]) %>%
  strsplit(", *") %>% unlist %>% table %>% sort

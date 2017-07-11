library(rentrez)
library(seqinr)
library(dplyr)
library(biogram)
library(pbapply)
library(mlr)

num_vals <- read.csv("./featureVisualisation/visual_data.csv")

raw_dat <- readLines("export_data_all_110117.csv") %>% 
  strsplit(split = "| ", fixed = TRUE) %>% 
  do.call(rbind, .)

dat <- raw_dat[-1, ]
colnames(dat) <- raw_dat[1, ]


for(single_name in dat[, "Name"]) {
  gene_ids <- entrez_search(db = "nucleotide", term = paste0(single_name, "[ORGN]", " mcrA[TITL]"))
  
  if(gene_ids[["count"]] > 0) {
    genes <- entrez_fetch(db = "nucleotide", id = gene_ids[["ids"]], rettype = "fasta")
    cat(genes, file = paste0("mcrA/", sub(" ", " ", single_name), ".fasta"))
  }
}

ngrams_matrix <- do.call(rbind, pblapply(list.files("mcrA/"), function(single_file) {
  seqs <- read.fasta(paste0("mcrA/", single_file))
  
  better_seqs <- seqs[lapply(seqs, table) %>% sapply(length) > 1]

  lapply(better_seqs, function(single_seq) single_seq[single_seq != "n"]) %>% 
    list2matrix() %>%
    count_ngrams(4, u = c("a", "c", "g", "t"), scale = TRUE) %>% 
    as.matrix %>% 
    colMeans
}))

cont_feats <- c("GrowthDoublingTime", "GrowthRate", "maximalGrowthTemperature", 
                "maximumGrowthNACL", "maximumGrowthPH", "minimalGrowthTemperature", "minimumGrowthNACL", 
                "minimumGrowthPH", "optimalGrowthNACLMaximal", "optimalGrowthNACLMinimal", 
                "optimalGrowthPHMaximal", "optimalGrowthPHMinimal", "optimalGrowthTemperatureMaximal", 
                "optimalGrowthTemperatureMinimal")

cont_preds <- lapply(cont_feats, function(single_trait){
           pred_dat <- data.frame(Name = sub(".fasta", "", list.files("mcrA/"), fixed = TRUE),
                                  ngrams_matrix) %>% 
             inner_join(select_(num_vals, "Name", single_trait)) %>% 
             na.omit
           
           predict_par <- makeRegrTask(id = "Temp", 
                                       data = select(pred_dat, - Name), target = single_trait)
           
           learnerRF <- makeLearner("regr.randomForest")
           
           set.seed(15390)
           
           res <- crossval(learnerRF, predict_par, 10L)
           
           sqrt(mean(res[["measures.test"]][, "mse"]))
         })

pred_list <- lapply(cont_feats, function(single_trait){
  pred_dat <- data.frame(Name = sub(".fasta", "", list.files("mcrA/"), fixed = TRUE),
                         ngrams_matrix) %>% 
    inner_join(select_(num_vals, "Name", single_trait)) %>% 
    na.omit
  
  predict_par <- makeRegrTask(id = "Temp", 
                              data = select(pred_dat, - Name), target = single_trait)
  
  learnerRF <- makeLearner("regr.randomForest")
  
  train(learnerRF, predict_par)
})

save(pred_list, file = "./NGramAnalyzer/pred_list.RData")


library(xlsx)

nice_names <- readLines("full_names.txt") %>% 
  strsplit('<OPTION value=\"', fixed = TRUE) %>% 
  first %>% 
  strsplit('\" > ', fixed = TRUE) %>% 
  unlist %>% 
  strsplit('\t\t\t\t\t', fixed = TRUE) %>% 
  unlist %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  data.frame(row.names = .[, 1]) %>% 
  rename(Property = X1, nice = X2)


data.frame(Property = cont_feats, Error = unlist(cont_preds)) %>% 
  mutate(Error = round(Error, 2)) %>%
  inner_join(nice_names) %>% 
  mutate(Property = nice) %>% 
  select(-nice) %>% 
  write.csv(file = "res.csv")

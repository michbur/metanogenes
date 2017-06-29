cont_feats <- c("GrowthDoublingTime", "GrowthRate", "maximalGrowthTemperature", 
                "maximumGrowthNACL", "maximumGrowthPH", "minimalGrowthTemperature", "minimumGrowthNACL", 
                "minimumGrowthPH", "optimalGrowthNACLMaximal", "optimalGrowthNACLMinimal", 
                "optimalGrowthPHMaximal", "optimalGrowthPHMinimal", "optimalGrowthTemperatureMaximal", 
                "optimalGrowthTemperatureMinimal")

extract_ngrams <- function(x) {
  list2matrix(x) %>%
    count_ngrams(4, u = c("a", "c", "g", "t"), scale = TRUE) %>% 
    as.matrix %>% 
    data.frame
}

pred_vals <- function(models, ngrams) {
  res <- lapply(models, function(single_model)
    predict(single_model[["model"]], newdata = ngrams)
  ) %>% 
    data.frame %>% 
    t %>% 
    data.frame(Property = cont_feats, .)
  
  rownames(res) <- NULL
  
  res
}


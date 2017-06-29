library(dplyr)

tmp <- readxl::read_excel("correct_pubs.xlsx") %>% 
  select(organism, `correct publication`, `correct publication link`) %>% 
  rename(Name = organism)

filter(tmp, is.na(`correct publication`)) %>% 
  select(Name) %>% 
  na.omit

write.csv(tmp, "correct_pubs.csv", row.names = FALSE)

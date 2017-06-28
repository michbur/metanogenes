library(dplyr)

tmp <- readxl::read_excel("baza metanogenów ze strony.xlsx") %>% 
  select(organism, `correct publication`, `correct publication link`) %>% 
  rename(Name = organism)

filter(tmp, is.na(`correct publication`)) %>% 
  select(Name) %>% 
  na.omit

write.csv(tmp, "correct_pubs.csv", row.names = FALSE)

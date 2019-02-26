############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")
source("functions/functions_NLP.R")

M1 <- readRDS("temp/M_1.RDS")
M2 <- readRDS("temp/M_nw.RDS")

M1 %<>% select(EID, DI, SR) %>%
  left_join(M2 %>% select(EID, com), by = "EID") 

write_csv(M1, "temp/IDs_corpus.csv")

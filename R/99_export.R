############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")
source("functions/functions_NLP.R")

M1 <- readRDS("temp/M_comleted.RDS") %>% select(EID, SR)
M2 <- readRDS("temp/M_nw.RDS") %>% select(EID, com, com2)

M1 %<>% 
  left_join(M2, by = "EID") %>%
  distinct(EID, .keep_all = TRUE)

write_csv(M1, "temp/IDs_corpus.csv")

library(tidygraph)
g <- readRDS("temp/g_bib.RDS") 


el <- g %E>% 
  mutate(i = .N()$name[from],
         j = .N()$name[to]) %>% 
  as_tibble() %>%
  select(i, j, weight, weight.count)

write_csv(el, "temp/edgelist_biblio.csv")

############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

# Extra packages
require(tidygraph)
require(ggraph)

############################################################################
# Load data
############################################################################

#
# TODO: Integrate SR in CR !!!!!!!
#

M <- readRDS("temp/M_nw.RDS")
cit_el <- readRDS("temp/cit_el.RDS")

### Inspect and subset
M %>% skim()

M %<>% 
  filter(TC >= 10 | TC_year >= 5) %>%
  filter(PY >= 1980 & PY <= 2019) %>%
  filter(NR >= 10 & NR <= 750) %>%
  filter( !(DT %in% c("Review", "Conference Paper")) )
  



TI <- M %>% select(com, AU1, PY, SO, TI, dgr_int) %>%
  group_by(com) %>%
  arrange(desc(dgr_int)) %>%
  slice(1:10) %>%
  ungroup()

SO <- M %>% select(com, SO, dgr_int) %>%
  group_by(com, SO) %>%
  summarise(dgr_int = sum(dgr_int)) %>%
  ungroup() %>%
  group_by(com) %>%
  arrange(desc(dgr_int)) %>%
  slice(1:10) %>%
  ungroup()

AU <- M %>% select(com, dgr_int, N_AU, AU) %>%
  mutate(dgr_int_frac = dgr_int / N_AU) %>%
  unnest(AU) %>%
  drop_na(AU) %>%
  group_by(com, AU) %>%
  summarise(dgr_int_frac = sum(dgr_int_frac)) %>%
  ungroup() %>%
  group_by(com) %>%
  arrange(desc(dgr_int_frac)) %>%
  slice(1:10) %>%
  ungroup() %>%
  select(com, AU, dgr_int_frac)

C1 <- M %>% select(com, dgr_int, C1) %>%
  unnest(C1) %>%
  unnest(C1) %>%
  drop_na(C1) %>%
  group_by(com, C1) %>%
  summarise(dgr_int = sum(dgr_int)) %>%
  ungroup() %>%
  group_by(com) %>%
  arrange(desc(dgr_int)) %>%
  slice(1:10) %>%
  ungroup() %>%
  select(com, C1, dgr_int)

SC <- M %>% select(com, dgr_int, SC) %>%
  unnest(SC) %>%
  drop_na(SC) %>%
  group_by(com, SC) %>%
  summarise(dgr_int = sum(dgr_int)) %>%
  ungroup() %>%
  group_by(com) %>%
  arrange(desc(dgr_int)) %>%
  slice(1:10) %>%
  ungroup() %>%
  select(com, SC, dgr_int)

DE <- M %>% select(com, dgr_int, DE) %>%
  unnest(DE) %>%
  drop_na(DE) %>%
  group_by(com, DE) %>%
  summarise(dgr_int = sum(dgr_int)) %>%
  ungroup() %>%
  group_by(com) %>%
  arrange(desc(dgr_int)) %>%
  slice(1:10) %>%
  ungroup() %>%
  select(com, DE, dgr_int)

CR <- M %>% select(com, dgr_int, CR) %>%
  unnest(CR) %>%
  mutate(CR = paste0(CR_AU1, " (" , CR_PY, ") ", CR_SO %>% str_trunc(35) ) )  %>%
  drop_na(CR_SID) %>%
  group_by(com, CR) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(com) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ungroup() %>%
  select(com, CR, n)

bib.sum <- AU %>% select(com, AU) %>%
  bind_cols(SO %>% select(SO)) %>%
  bind_cols(C1 %>% select(C1)) %>% 
  bind_cols(SC %>% select(SC)) %>% 
  bind_cols(DE %>% select(DE)) %>% 
  bind_cols(CR %>% select(CR, com)) 


x <- M %>%
  count(com, com2)

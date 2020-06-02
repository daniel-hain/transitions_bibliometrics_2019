############################################################################
# TODO list
############################################################################

# Download infors on !: Authors, 2: Institutions, 3: Journals

############################################################################
# Preamble
############################################################################
### Generic preamble
rm(list=ls())

Sys.setenv(LANG = "en") # For english language
options(scipen = 5) # To deactivate annoying scientific number notation
set.seed(1337) # To have a seed defined for reproducability

library(tidyverse)
library(magrittr)

############################################################################
# Select Seed articels
############################################################################

# own functions
source("functions/functions_scopus.R")

file_list <- read_csv("../input/000_seed_index.csv")
index <- file_list %>% na_if("") %>% drop_na(index) %>% distinct(index)  %>% pull() 
index <- paste0("../input/", index, ".txt")

M <- read_scopus_collection(index, TC_min = 1, TC_year_min = 0.25, PY_max = 2019, PY_min = 1998, n_max = 1000, 
                            type = "reduced", exclude = c(DT = "Conference Paper")) %>% arrange(desc(TC), PY)

saveRDS(M, "../temp/M_1.RDS")


#####################################################################################################
# Download adittional info from scopus:
#####################################################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")
library(httr)
library(jsonlite)
library(rlist)

scopus_key <- "79bfa1706e6434d1e36992889ebeb3d5" # or: 	"60c71dc8201d9e13dfac7872fb340130"

### Abstract update
M <- readRDS("../temp/M_1.RDS")
M.scopus_full <- readRDS("../output/M_scopus_full_raw.RDS")

EID_select <- M %>% select(EID)
EID_exist <- M.scopus_full %>% list.select(x = coredata$eid) %>% map(unname) %>% unlist() 
EID_select %<>% filter( !(EID %in% EID_exist) ) %>% pull(EID)

M.scopus_addon <- scopus_document_ID(ID = EID_select, idtype = "eid", type = "abstract", view = "FULL", scopus_key = scopus_key, start = 1, t_limit = 6)
M.scopus_addon <- M.scopus_addon[!sapply(M.scopus_addon, is.null)]

M.scopus_addon %<>% map("abstracts-retrieval-response")
M.scopus_full %<>% append(M.scopus_addon)

saveRDS(M.scopus_full, "output/M_scopus_full_raw.RDS")

#####################################################################################################
# Process adittional Scopus infos
#####################################################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")
library(rlist)

M <- readRDS("temp/M_1.RDS")
M.scopus_full <- readRDS("output/M_scopus_full_raw.RDS") 

# Reduce to only use what we need
EID_select <- M %>% pull(EID) 
EID_all <- M.scopus_full %>% list.select(x = coredata$eid) %>% map(unname) %>% unlist() 
EID_restricted <- (EID_all %in% EID_select)
M.scopus_full <- M.scopus_full[EID_restricted]
rm(EID_select, EID_all, EID_restricted)

EID_index <- M.scopus_full %>% list.select(x = coredata$eid) %>% map(unname) %>% unlist() 


# Author 
AU <- scopus_extract_AU(M.scopus_full, index = EID_index, level = "document")

# Institution
C1 <- scopus_extract_C1(M.scopus_full, index = EID_index, level = "document")

# Subject Area 
SC <- scopus_extract_SC(M.scopus_full, index = EID_index, level = "document")

# Funding 
FX <- scopus_extract_FX(M.scopus_full, index = EID_index, level = "document")

# General infos
MX <- scopus_extract_MX(M.scopus_full, index = EID_index, level = "document")
  

#######################
##### Citations
#######################

##### Citation edgelist
cit_el <- M.scopus_full %>%
  list.select(REF = item$bibrecord$tail$bibliography$reference) %>%
  map("REF")  %>% map_depth(2, "ref-info") %>% 
  {tibble(EID = EID_index,
          CR_SID  = map_depth(.,2, "refd-itemidlist") %>% map_depth(2, "itemid") %>% map_depth(2, "$") %>%
            map_depth(2, replace_NULL) %>% map(unlist),
          CR_PY = map_depth(.,2, "ref-publicationyear") %>% map_depth(2, "@first") %>%
            map_depth(2, replace_NULL) %>% map(unlist),
          CR_TI  = map_depth(., 2, "ref-title") %>% map_depth(2, "ref-titletext") %>%
            map_depth(2, replace_NULL) %>% map(unlist),
          CR_SO  = map_depth(., 2, "ref-sourcetitle") %>%
            map_depth(2, replace_NULL) %>% map(unlist),
          CR_AU  = map_depth(., 2, "ref-authors") %>%  map_depth(2, "author") %>%  map_depth(3, "ce:indexed-name") %>% 
            map_depth(3, replace_NULL) %>% map_depth(2, unlist) # Note : Paste nly till dplyr fixed
          ) } %>%
  replace_NULL() 

cit_el %<>%
  unnest() %>%
  mutate(CR_SID = as.character(CR_SID) %>% str_squish(),
         CR_PY = CR_PY %>% as.character() %>% as.numeric(),
         CR_TI = as.character(CR_TI) %>% str_squish(),
         CR_SO = as.character(CR_SO) %>% str_squish()) %>%
  drop_na(EID, CR_SID) %>%
  distinct(EID, CR_SID, .keep_all = TRUE) %>%
  filter(EID %in% (M %>% pull(EID)) ) %>%
  group_by(CR_SID) %>% mutate(CR_TC = n()) %>% ungroup() %>% filter(CR_TC >= 2) %>%
  group_by(EID) %>% mutate(NR = n()) %>% ungroup() %>% filter(NR >= 2) 


cit_el %<>%
  mutate(CR_AU = CR_AU %>% map(replace_NULL)) %>%
  rename(AU = CR_AU) %>%
  unnest(AU) %>%
  clean_AU() %>% 
  nest(AU, .key = "AU") %>%
  mutate(N_AU = map(AU, lengths) %>% unlist() %>% as.character() %>% as.numeric() ) %>%
  group_by(EID, CR_SID) %>% gen_AU1() %>% ungroup() %>%
  rename(CR_N_AU = N_AU, 
         CR_AU = AU,
         CR_AU1 = AU1) %>%
  replace_NULL() %>%
  drop_na(EID, CR_SID) %>%
  select(EID, CR_SID, CR_PY, CR_AU1, CR_TC, NR, CR_N_AU, CR_TI, CR_SO, CR_AU)

cit_el %<>%
  drop_na(EID, CR_SID, CR_PY)

  
saveRDS(cit_el, "temp/cit_el.RDS")

##### Citations
C <- cit_el %>%
  select(-EID, -NR) 

colnames(C) <- colnames(C) %>% str_remove_all("CR_")

C %<>% 
  arrange(desc(TC)) %>%
  distinct(SID, .keep_all = TRUE) %>%
  mutate(SR = paste0(AU1, " (" , PY, ") ", SO %>% str_trunc(25) ) )  

saveRDS(C, "temp/C_data.RDS")

##### Citation Author
CR_AU <- C %>%
  select(SID, PY, TC, AU, N_AU) %>%
  unnest() %>%
  replace_NULL() %>%
  clean_AU() %>%
  mutate(TC_frac = TC / N_AU,
         N_frac = 1 / N_AU) %>%
  replace_NULL() %>%
  group_by(AU) %>%
  mutate(AU_N = n(),
         AU_N_frac = sum(N_frac),
         AU_TC = sum(TC),
         AU_TC_frac = sum(TC_frac) ) %>%
  ungroup() %>%
  drop_na(SID, AU) %>%
  nest(SID, PY, N_AU, N_frac, TC, TC_frac, .key = AU_PUB)

saveRDS(CR_AU, "temp/CR_AU_data.RDS")

#####################################################################################################
# Final Join and save
#####################################################################################################
rm(M.scopus_full)

CR <- cit_el %>%
  nest(-EID, -NR, .key = "CR") %>%
  replace_NULL() 

merge <- tibble(EID = (M %>% pull(EID)) ) %>%
  left_join(MX , by = "EID") %>%
  left_join(AU , by = "EID") %>%
  inner_join(CR, by = "EID") %>%
  left_join(C1, by = "EID") %>%
  left_join(FX , by = "EID") %>% 
  left_join(SC , by = "EID") %>% 
  replace_NULL() %>% distinct(EID, .keep_all = TRUE) 

M %<>% 
  select(-AU, -AU1, -AU_NR) %>%
  inner_join(merge, by = "EID") %>%
  replace_NULL() %>% distinct(EID, .keep_all = TRUE) 

# Last small things
M %<>%
  mutate(SR = paste0(AU1, ", (", PY, ") ", (SO %>% str_trunc(50)) ))

M %<>%
  select(EID, SID, SR, PY, AU1, SO, TI, TC, TC_year, NR, N_AU, DE, ID, SC, DT, AU, CR, C1, FX) 
  
saveRDS(M, "temp/M_comleted.RDS")


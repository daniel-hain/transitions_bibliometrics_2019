
############################################################################
# TODO list
############################################################################

# Download infors on !: Authors, 2: Institutions, 3: Journals

############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

############################################################################
# Select Seed articels
############################################################################

rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")

file_list <- c("input/scopus_search_string.csv", "input/scopus_search_manual.csv")

M.base <- read_scopus_csv_collection(file_list, TC_min = 1, TC_year_min = 0.25, PY_max = 2019, PY_min = 1970)

# Define which ones we want to manually deselect
select.neg <- c("2-s2.0-33845930247", # "TRANSITIONS TOWARDS ADAPTIVE MANAGEMENT OF WATER FACING CLIMATE AND GLOBAL CHANGE",
                "2-s2.0-0347682362", # "TECHNOLOGY ROADMAPPING - A PLANNING FRAMEWORK FOR EVOLUTION AND REVOLUTION",
                "2-s2.0-84857938593", #"GLOBAL ENVIRONMENTAL CHANGE II: FROM ADAPTATION TO DELIBERATE TRANSFORMATION",
                "2-s2.0-84922465409", #"RECONCEPTUALISING ADAPTATION TO CLIMATE CHANGE AS PART OF PATHWAYS OF CHANGE AND RESPONSE",
                "2-s2.0-84861676296", #"OVERCOMING THE TRAGEDY OF SUPER WICKED PROBLEMS: CONSTRAINING OUR FUTURE SELVES TO AMELIORATE GLOBAL CLIMATE CHANGE",
                "2-s2.0-84938943029", #"ECOLOGY IN AN ANTHROPOGENIC BIOSPHERE",
                "2-s2.0-84977513357" # "ENERGY, LAND-USE AND GREENHOUSE GAS EMISSIONS TRAJECTORIES UNDER A GREEN GROWTH PARADIGM"
                )

# Selection of seed articles
select1 <- M.base %>%
  filter(!(EID %in% select.neg)) %>%
  filter(TC >= quantile(.$TC,0.98) | (TC_year >= quantile(.$TC_year,0.98) ) ) 

select2 <- M.base %>%
  filter(!(EID %in% select.neg)) %>%
  filter(PY >= 2015 & PY < 2018)  %>% 
  arrange(PY, desc(TC)) %>%
  group_by(PY)%>%
  slice(1:3) %>%
  ungroup() 

select <- select1 %>%
  bind_rows(select2) %>%
  distinct(EID, .keep_all = TRUE) %>%
  arrange(desc(TC)) %>%
  select(EID, AU1, TI, JI, PY, TC, TC_year)

### First reduction for merging
M.base %<>%
  filter(TC >= 1) %>%
  filter( TC >= quantile(.$TC, 0.50) | 
            TC_year >= quantile(.$TC_year, 0.50) | 
            EID %in% (select %>% pull(EID) ) ) 

saveRDS(select, "output/seed_papers.RDS") 
saveRDS(M.base, "temp/M_base.RDS") 

rm(file_list, M.base, select1, select2, select.neg)

############################################################################
# Save all the Abstracts in SQL
############################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")

library(DBI)
library(RSQLite)
library(dbplyr)

### Load all abstracts available
file_list <- dir("input")
file_list <- file_list[grep("scopus.*\\.csv", file_list)] %>% paste("input", . , sep="/") 

M <- read_scopus_csv_collection(file_list, type = "AB")

# Some preprocessing
M %<>%
  mutate(AB = str_to_lower(AB)) %>%
  arrange(EID)

### Load in the sql
db <- dbConnect(RSQLite::SQLite(), "output/bliographics.sqlite")

dbExecute(db,"CREATE TABLE IF NOT EXISTS transitions_2019_AB (
                      EID char(30) PRIMARY KEY,
                      AB text) WITHOUT ROWID;" ) 

dbWriteTable(conn = db, 
             name = "transitions_2019_AB", 
             value = M, 
             append = TRUE,
             overwrite = FALSE,
             row.names = FALSE)

dbExecute(db, "CREATE INDEX IF NOT EXISTS transitions_2019_AB_EID ON transitions_2019_AB (EID);")

dbListTables(db)
#dbRemoveTable(db, "transitions_2019_AB") # If table needs to be removed
dbDisconnect(db)

############################################################################
# Select Seed articels
############################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")

file_list <- dir("input")
file_list <- file_list[grep("scopus.*[0-9]+.*\\.csv", file_list)] %>% paste("input", . , sep="/") 

M <- read_scopus_csv_collection(file_list, TC_min = 1, TC_year_min = 0.25, PY_max = 2019, PY_min = 1970, n_max = 500)

# Join with base string search
M %<>%
  select(-AB) %>%
  rbind(readRDS("temp/M_base.RDS") %>% select(-AB)) %>%
  arrange(desc(TC)) %>%
  distinct(EID, .keep_all = TRUE)

saveRDS(M, "temp/M_1.RDS")

#####################################################################################################
# Download adittional info from scopus:
#####################################################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")
library(httr)
library(jsonlite)
library(rlist)

M <- readRDS("temp/M_1.RDS")

EID_select <- M %>% select(EID)
scopus_key <- "7f59af901d2d86f78a1fd60c1bf9426a"

# ### FUll abstract retrieval
# M.scopus_full <- scopus_document_ID(ID = EID_select, idtype = "eid", type = "abstract", view = "FULL", scopus_key = scopus_key, start = 1, t_limit = 6)
# # # In case there is an error and you need to update
# # M.scopus_full <- M.scopus_full[lengths(M.scopus_full) != 0] %>% append(M.scopus_full.addon)
# # M.scopus_full %<>% map("abstracts-retrieval-response")
# # saveRDS(M.scopus_full, "temp/M.scopus_full_raw.RDS")

### Abstract update
M.scopus_full <- readRDS("output/M_scopus_full_raw.RDS")
EID_exist <- M.scopus_full %>% list.select(x = coredata$eid) %>% map(unname) %>% unlist() 
EID_select %<>% filter( !(EID %in% EID_exist) ) %>% pull(EID)

M.scopus_addon <- scopus_document_ID(ID = EID_select, idtype = "eid", type = "abstract", view = "FULL", scopus_key = scopus_key, start = 1, t_limit = 6)
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
EID_index <- M.scopus_full %>% list.select(x = coredata$eid) %>% map(unname) %>% unlist() 

### define some helper functions
subset_name <- function(x, select){ x <- subset(x, names(x) %in% select) }

replace_NULL <- function(x){ x <- x %>% replace(is.null(.), NA) %>% replace(.== "NULL", NA) %>% replace(.== "NA", NA) %>% 
  replace(.=="list()", NA) %>% replace(.=="character()", NA) %>% replace(.=="numeric()", NA) 
  }

clean_nested <- function(x) {
  x %<>%
    unnest() %>%
    mutate_all(as.character) %>% 
    mutate_if(is_character, str_squish) %>%
    replace_NULL()
  }

clean_AU <- function(x) {
  x %<>% 
    mutate(AU = str_squish(AU) ) %>%
    mutate(AU = str_remove_all(AU, "\\..*") ) %>%
    mutate(AU = ifelse(AU == "Dosi", "Dosi G", AU) )
    } # TODO: MAke it more generic

gen_AU1  <- function(x) {
  x %<>% 
    mutate(AU1 =  ifelse(N_AU == 0, NA,
                         ifelse(N_AU == 1, (AU %>% unlist())[1], 
                                ifelse(N_AU == 2, paste( (AU %>% unlist())[1], (AU %>% unlist())[2], sep = " & "), 
                                       paste( (AU %>% unlist())[1],"et al.", sep = " ") ) ) ) )
  }

# TODO: MAke them all in their numbvering more generic

#######################
##### Author 
#######################
AU <- M.scopus_full %>%
  list.select(AU = authors$author) %>%
  map("AU") %>% 
  {tibble(EID = EID_index,
          AU = map_depth(., 2, "ce:indexed-name") %>% map_depth(2, replace_NULL),            
          AU_ID = map_depth(., 2, "@auid") %>% map_depth(2, replace_NULL),
          AU_C1_ID = map_depth(., 2, "affiliation") %>% map_depth(2, "@id") %>% map_depth(2, replace_NULL)
          ) } %>% replace_NULL()

x <- AU %>%
  clean_nested() %>%
  clean_AU() %>%
  replace_NULL() %>%
  group_by(EID) %>%
  mutate(N_AU = n()) %>%
  filter(N_AU >= 1) %>%
  ungroup()

AU <- x %>% 
  nest(-EID, - N_AU, .key = "AU") %>%
  group_by(EID) %>% gen_AU1() %>% ungroup()
  select(EID, AU1, N_AU, AU)

AU_data <- x %>% 
  nest(-AU_ID) 

saveRDS(AU_data, "temp/AU_data.RDS")
rm(x, AU_data)

#######################
##### Affiliation TODO = Still not fully working
#######################
C1 <- M.scopus_full %>%
  map("affiliation") %>%
  {tibble(EID = EID_index,
          C1 = map(., "affilname"),
          C1_x = map_depth(., 2, "affilname") %>% map_depth(2, replace_NULL),
          C1_ID = map(., "@id"),
          C1_ID_x = map_depth(., 2, "@id") %>% map_depth(2, replace_NULL),
          C1_CT = map(., "affiliation-city"),
          C1_CT_x = map_depth(., 2, "affiliation-city") %>% map_depth(2, replace_NULL),
          C1_CN = map(., "affiliation-country"),
          C1_CN_x = map_depth(., 2, "affiliation-country") %>% map_depth(2, replace_NULL)
          ) } %>%
  group_by(EID) %>%
  mutate(C1    = ifelse(C1 != "NULL",  list(C1), C1_x),
         C1_ID = ifelse(C1_ID != "NULL",  list(C1_ID), C1_ID_x),
         C1_CT = ifelse(C1_CT != "NULL",  list(C1_CT), C1_CT_x),
         C1_CN = ifelse(C1_CN != "NULL",  list(C1_CN), C1_CN_x) ) %>%
  ungroup() %>%
  select(-C1_x, -C1_ID_x, -C1_CT_x, -C1_CN_x) %>%
  replace_NULL()

x <- tibble(EID = C1 %>% pull(EID)) %>%
  bind_cols(C1 %>% select(EID, C1) %>% clean_nested() %>% nest(-EID, .key = "C1") %>% select(-EID)) %>%
  bind_cols(C1 %>% select(EID, C1_ID) %>% clean_nested() %>% nest(-EID, .key = "C1_ID") %>% select(-EID)) %>%
  bind_cols(C1 %>% select(EID, C1_CT) %>% clean_nested() %>% nest(-EID, .key = "C1_CT") %>% select(-EID)) %>%
  bind_cols(C1 %>% select(EID, C1_CN) %>% clean_nested() %>% nest(-EID, .key = "C1_CN") %>% select(-EID) ) 

C1 <- x %>% nest(-EID, .key = "C1")

C1_data <- x %>% 
  select(C1_ID, EID) %>%
  unnest() %>%
  drop_na() %>%
  nest(-C1_ID)
  
saveRDS(C1_data, "temp/C1_data.RDS")
rm(x, C1_data)

#######################
##### Subject Area 
#######################
SC <- M.scopus_full %>%
  list.select(SC = `subject-areas`$`subject-area`) %>%
  map("SC") %>% 
  {tibble(EID = EID_index,
          SC = map_depth(., 2, flatten) %>% map_depth(2, "$") %>% map_depth(2, replace_NULL) %>% map(unlist),
          SC_ID = map_depth(., 2, unlist) %>% map_depth(2, "@code") %>% map_depth(2, replace_NULL) %>% map(unlist),
          SC_CODE = map_depth(., 2, unlist) %>% map_depth(2, "@abbrev") %>% map_depth(2, replace_NULL) %>% map(unlist) 
          ) } %>% replace_NULL()

SC %<>% clean_nested() %>%
  nest(-EID, .key = "SC")

#######################
##### General infos
#######################
MX <- M.scopus_full %>%
  list.select(MX = coredata) %>%
  map("MX") %>% 
  {tibble(EID = EID_index,
          SO_ID = map(., "source-id") 
          ) } %>%
  mutate(SO_ID = as.character(SO_ID))%>% 
  replace_NULL() 
ID <- M.scopus_full %>%
  list.select(ID = item$bibrecord$`item-info`$itemidlist$itemid) %>%
  map("ID") %>% 
  {tibble(EID = EID_index,
          SID = map_depth(., 2, unlist) %>% map_depth(2, function(x){paste(x,collapse="")})  %>% map(unlist) %>%
            map(list.filter, grepl("SGR",.)) %>% map(str_remove, "SGR") %>% map(unlist)
  ) }  %>% 
  mutate(SID = as.character(SID)) %>% 
  replace_NULL()
MX %<>% 
  left_join(ID, by = "EID") %>% distinct(EID, .keep_all = TRUE) 
rm(ID)

#######################
### Funding 
#######################
FX <- M.scopus_full %>%
  list.select(FX = item$`xocs:meta`$`xocs:funding-list`) %>%
  map("FX") %>% 
  {tibble(EID = EID_index,
          FX = map(., "xocs:funding") %>% map("xocs:funding-agency-matched-string"),
          FX_CODE = map(., "xocs:funding") %>% map("xocs:funding-agency-acronym"),
          FX_ID = map(., "xocs:funding") %>% map("xocs:funding-agency-id"),
          FX_CN = map(., "xocs:funding") %>% map("xocs:funding-agency-country"),
          FX_TXT = map(., "xocs:funding-text")
          ) } %>% replace_NULL()

FX %<>%
  mutate_all(as.character) %>% 
  mutate_if(is_character, str_squish) %>%
  replace_NULL() %>% 
  nest(-EID, .key = "FX")

#######################
##### Citations
#######################

##### Citation edgelist
cit_el <- M.scopus_full %>%
  list.select(REF = item$bibrecord$tail$bibliography$reference) %>%
  map("REF")  %>% map_depth(2, "ref-info") %>% 
  {tibble(EID = EID_index,
          CR_SID  = map_depth(.,2, "refd-itemidlist") %>% map_depth(2, "itemid") %>% map_depth(2, "$") %>%
            map_depth(2, replace_NULL) %>% map(flatten),
          CR_PY = map_depth(.,2, "ref-publicationyear") %>% map_depth(2, "@first") %>%
            map_depth(2, replace_NULL) %>% map(flatten),
          CR_TI  = map_depth(., 2, "ref-title") %>% map_depth(2, "ref-titletext") %>%
            map_depth(2, replace_NULL) %>% map(flatten),
          CR_SO  = map_depth(., 2, "ref-sourcetitle") %>%
            map_depth(2, replace_NULL) %>% map(flatten),
          CR_AU  = map_depth(., 2, "ref-authors") %>%  map_depth(2, "author") %>%  map_depth(3, "ce:indexed-name") %>% 
            map_depth(3, replace_NULL) %>% map_depth(2, flatten) # Note : Paste nly till dplyr fixed
          ) } 

cit_el %<>%
  replace_NULL() %>%
  unnest() %>%
  mutate(CR_SID = as.character(CR_SID) %>% str_squish(),
         CR_PY = CR_PY %>% as.character(),
         CR_TI = as.character(CR_TI) %>% str_squish(),
         CR_SO = as.character(CR_SO) %>% str_squish()) %>%
  drop_na(EID, CR_SID) %>%
  distinct(EID, CR_SID, .keep_all = TRUE) %>%
  filter(EID %in% (M %>% pull(EID)) ) %>%
  group_by(CR_SID) %>% mutate(CR_TC = n()) %>% ungroup() %>% filter(CR_TC >= 2) %>%
  group_by(EID) %>% mutate(NR = n()) %>% ungroup() %>% filter(NR >= 2) 

cit_el %<>%
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
  select(-AU, -AU1, -AU_ID, -NR, -FU, -FX, -AU_NR) %>%
  inner_join(merge, by = "EID") %>%
  replace_NULL() %>% distinct(EID, .keep_all = TRUE) 

# Last nice cleanup (TODO: You can already restrict that earlier)
x <- M %>%
  select(EID, SID, JI, SO_ID, IS, VL, PN, BP, EP, DI, BN, SN, OS, PU, BE) %>%
  nest(JI, SO_ID, IS, VL, PN, BP, EP, DI, BN, SN,  OS, PU, BE, .key = "MX")

saveRDS(x, "temp/M_bib_info.RDS")

M %<>%
  select(EID, SID, SR, PY, AU1, SO, TI, TC, TC_year, NR, N_AU, DE, ID, SC, DT, AU, CR, C1, FX) 
  
saveRDS(M, "temp/M_comleted.RDS")



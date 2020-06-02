############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

############################################################################
# Select Seed articels
############################################################################

rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")

file_list <- c("input/00_search_string.txt", "input/00_seed.txt")

M.base <- read_scopus_collection(file_list, TC_min = 1, TC_year_min = 0, PY_max = 2019, PY_min = 1998, 
                                 type = "reduced", exclude = c(DT = "Conference Paper")) 


# adjust TC to 2018
M.base %<>%
  arrange(desc(TC)) %>%
  mutate(PY = ifelse(PY > 2018, 2018, PY)) %>%
  mutate(TC_year = TC / (2018 - PY + 1)  )

# Export top-100 TC
M.TC <- M.base %>% 
  arrange(desc(TC)) %>%
  mutate(percentile_TC = (rank(TC) / n()) %>% round(2),
         percentile_TCyear = (rank(TC_year) / n()) %>% round(2),
         AU = AU %>% as.character()) %>%
  select(EID, TC, percentile_TC, TC_year, percentile_TCyear, AU, PY, TI, SO) %>%
  slice(1:100) 
write_csv(M.TC, "temp/string_top100_TC.csv")

# Export top-100 TC/year
M.TC_year <- M.base %>% 
  arrange(desc(TC_year)) %>%
  mutate(percentile_TC = (rank(TC) / n()) %>% round(2),
         percentile_TCyear = (rank(TC_year) / n()) %>% round(2),
         AU = AU %>% as.character()) %>%
  select(EID, TC, percentile_TC, TC_year, percentile_TCyear, AU, PY, TI, SO) %>%
  slice(1:100) 
write_csv(M.TC_year, "temp/string_top100_TCyear.csv")
rm(M.TC,M.TC_year)

### First reduction for merging
M.base %<>%
  filter( TC >= quantile(.$TC, 0.50) | TC_year >= quantile(.$TC_year, 0.50) ) 

saveRDS(M.base, "temp/M_base.RDS") 

############################################################################
# Save all the Abstracts in SQL
############################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")

library(DBI)
library(RSQLite)
library(dbplyr)

### Load all abstracts available
index <- list.files(path = "input", pattern = "\\.txt", all.files = FALSE, full.names = TRUE)

M <- read_scopus_collection(index, fields = c("EID", "PY", "TI", "DE", "ID", "AB") )

M %<>%
  mutate(DE = DE %>% map_chr(paste, collapse = "; "),
         ID = ID %>% map_chr(paste, collapse = "; ")) 
  
# Some preprocessing
M %<>% 
  drop_na(EID, AB) %>%
  arrange(EID) 

### Load in the sql
db <- dbConnect(RSQLite::SQLite(), "output/bliographics.sqlite")
# dbRemoveTable(db, "transitions_2019_AB") # If table needs to be removed

dbExecute(db,"CREATE TABLE IF NOT EXISTS transitions_2019_AB (
                  EID char(30) PRIMARY KEY,
                  PY int,
                  TI text,
                  DE text,
                  ID text,
                  AB text) WITHOUT ROWID;" ) 

dbWriteTable(conn = db, 
             name = "transitions_2019_AB", 
             value = M, 
             append = TRUE,
             overwrite = FALSE,
             row.names = FALSE)

dbExecute(db, "CREATE INDEX IF NOT EXISTS transitions_2019_AB_EID ON transitions_2019_AB (EID);")

dbListTables(db)
dbDisconnect(db)

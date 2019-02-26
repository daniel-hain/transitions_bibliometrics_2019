############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

############################################################################
# Select Seed articels
############################################################################

rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")


test <- fread("input/scopus_geels2004.txt")

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
            EID %in% (select %>% pull(EID) )  ) 

saveRDS(select, "output/seed_papers.RDS") 
saveRDS(M.base, "temp/M_base.RDS") 


# for now:
M.select <- M.base %>%
  select(EID, AU1, PY, TI, SO, TC, TC_year) %>%
  mutate(seed_current = EID %in% (select %>% pull(EID)) %>% as.numeric()) %>%
  arrange(desc(seed_current), desc(TC), (desc(TC_year))) %>%
  slice(1:100)
write_csv(M.select, "temp/seed_select.csv")


############################################################################
# Save all the Abstracts in SQL
############################################################################
rm(list=ls()); graphics.off()
source("functions/functions_scopus.R")

library(DBI)
library(RSQLite)
library(dbplyr)

### Load all abstracts available
file_list <- fread("input/000_seed_index.csv")
index <- file_list %>% na_if("") %>% drop_na(index) %>% distinct(index)  %>% pull() 
index <- paste0("input/", index, ".txt")

M <- fread(index[1])

M <- read_scopus_collection(index, fields = c("EID", "TI", "DE", "ID", "AB") )

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

############################################################################
# Preamble
############################################################################

source("C:/Users/Admin/R_functions/preamble.R")
source("functions/functions_summary.R")

###########################################################################
# Get data out of SQL
############################################################################

# DB SQL packages
library(DBI)
library(RSQLite)
library(dbplyr)

db <- dbConnect(RSQLite::SQLite(), "output/bliographics.sqlite")
ab <- tbl(db, "transitions_2019_AB")

data <- ab %>% collect()
dbDisconnect(db)
rm(db, ab)

###########################################################################
# Some first preprocessing
###########################################################################

## Some preprocessing
# TODO: MAke a proper stem-completion instead
data %<>% 
  mutate(TXT = paste(TI, DE, AB, sep = " ")) %>% 
  mutate(TXT = TXT %>% 
           str_replace_all("&", "-and-") %>%
           str_remove_all("/(&trade;|&reg;|&copy;|&#8482;|&#174;|&#169;)/.*") %>%
           str_squish() %>%
           iconv(to = "UTF-8", sub = "byte") %>%
           str_remove("�.*") 
  )  %>%
  select(EID, PY, TXT) %>%
  drop_na()
data %<>%
  mutate(TXT = TXT %>% tolower()) 


data %<>% 
  mutate(Theory =  str_detect(TXT, "theor.*|concept|framework") & str_detect(TXT, "improv.*|develop.*"),
         Governance = str_detect(TXT, "transition management|governance"),
         Politics = str_detect(TXT, "polit.*|power"),
         Geography = str_detect(TXT, "urban.*|spatial|city|cities|geograph.*"),
         Buisness = str_detect(TXT, "firm|industr.*|actor|organization|sector") & str_detect(TXT, "strateg.*|business"),
         Social = str_detect(TXT, "social movement|communit.*|grassroot|civil society"),   
         Practices = (str_detect(TXT, "consumption") & str_detect(TXT, "practice")) | str_detect(TXT, "practice theory|everyday") ,  
         Ethics = str_detect(TXT, "ethic.*|justice|poverty")   
         )

data %<>%
  select(-TXT) %>%
  gather(key = "content", value = "present", Theory:Ethics)%>%
  arrange(EID, content)

data %<>%
  group_by(PY, content) %>%
  summarise(n = sum(present)) %>%
  ungroup() %>%
  group_by(PY) %>%
  mutate(n.rel = n / sum(n)) %>%
  ungroup()


data %>% plot_summary_timeline(y1 = n, y2 = n.rel, by = content, pal = "Paired", label = TRUE, 
                               y1_text = "Publications annualy", y2_text = "Share of publications annually",
                               PY_min = 2009, PY_max = 2018)

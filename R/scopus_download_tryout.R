
############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")
library(httr)
library(jsonlite)
library(rlist)
# devtools::install_github("muschellij2/rscopus")

M <- readRDS("temp/M1.RDS")

scopus_key <- "7f59af901d2d86f78a1fd60c1bf9426a"

EID_select <- M %>% pull(EID)
ID <- EID_select[1:20]

test <- scopus_document_ID(ID, idtype = "eid", type = "abstract", view = "FULL", scopus_key = scopus_key, start = 1, t_limit = 6)


# C <- tibble()
for(i in (nrow(C)+ 1):length(urls)){
  x <- fromJSON(urls[i], simplifyDataFrame = TRUE, flatten = TRUE)
  y <- tibble(EID = EID_select[i], CIT_EID = list(x$`abstracts-retrieval-response`$references$reference$`scopus-eid`))
  C %<>% rbind(y)
  Sys.sleep(0.33)
}


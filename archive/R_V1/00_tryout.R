
library(TidyScientometrix)

source("C:/Users/Admin/R_functions/preamble.R")

# Data: Selected fields for type = "reduced"
select_reduced <- c("EID", "AU", "PY", "TI", "SO", "VL", "IS", "TC", "DE", "ID", "DT")


file_list <- fread("input/000_seed_index.csv")
index <- file_list %>% na_if("") %>% drop_na(index) %>% distinct(index)  %>% pull()
index <- paste0("input/", index, ".txt")

index <- index[1:5]

M <- read_scopus_collection(index, TC_min = 1, TC_year_min = 0.25, PY_max = 2019, PY_min = 1998, n_max = 1000,
                            type = "reduced", exclude = c(DT = "Conference Paper")) %>% arrange(desc(TC), PY)



### Generic preamble
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path ))

Sys.setenv(LANG = "en") # For english language
options(scipen = 5) # To deactivate annoying scientific number notation
set.seed(1337) # To have a seed defined for reproducability

# source("../R/functions/functions_scopus.R")
library(tidyverse)
library(magrittr)

library(rscopus)

x = abstract_retrieval("S1053811915002700", identifier = "pii",
                       verbose = FALSE)

res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE,
                api_key ="60c71dc8201d9e13dfac7872fb340130")



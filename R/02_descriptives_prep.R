############################################################################
# Preamble
############################################################################
### Generic preamble
rm(list=ls())

options(scipen = 5) # To deactivate annoying scientific number notation
set.seed(1337) # To have a seed defined for reproducability

# own functions
source("functions/functions_basic.R")

### Load standard packages
library(tidyverse)
library(magrittr)

### Extra packages
#library(bibliometrix)
#library(tidygraph)

###########################################################################
# General
###########################################################################
rm(list=ls())

M <- readRDS("../temp/M.RDS")
M_bib <- readRDS("../temp/M_bib.RDS")
results <- readRDS("../temp/results.RDS")

M %<>% inner_join(M_bib, by = 'X') 

# GEnertal results
results %>% summary(k = 10, pause = FALSE)
results %>% plot(k = 10, pause = FALSE)


###########################################################################
# Network Bibliographic coupling
###########################################################################
g_bib <- readRDS("../temp/g_bib.RDS")

# inspect
com_bib <- g_bib %N>% as_tibble() %>% group_by(com) %>% slice_max(order_by = dgr_int, n = 10)
g_bib %N>% as_tibble() %>% count(com)

###########################################################################
# Network cocitation
###########################################################################
g_cit <- readRDS("../temp/g_cit.RDS")

# inspect
com_cit <- g_cit %N>% as_tibble() %>% group_by(com) %>% slice_max(order_by = dgr_int, n = 10)
g_cit %N>% as_tibble() %>% count(com)

###########################################################################
# NLP
###########################################################################
### Generic preamble


###########################################################################
# HAdittional fun
############################################################################





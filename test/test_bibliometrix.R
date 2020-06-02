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

### Extra packages
library(bibliometrix)

##################################################################
##	Functions
##################################################################

read_collection <- function(file_list, bib_source, bib_format, TC_min = 0, PY_min = 0, PY_max = Inf, n_max = Inf) {
  require(bibliometrix); require(dplyr); require(magrittr)
  
  x <- data.frame()
  for(i in 1:length(file_list)) {# i = 4; bib_source = 'scopus'; bib_format = 'csv'; TC_min = 0; PY_min = 1990; PY_max = Inf; n_max = 1000
    cat("===== Loading bibliometric dataframe", i, "out of", length(file_list), "=====\n", sep = " ")
    
    y <- convert2df(file = file_list[i], dbsource = bib_source, format = bib_format) %>%
      rownames_to_column('XX') %>%
      filter(TC >= TC_min & PY >= PY_min & PY <= PY_max) %>%
      slice(1:n_max)
    # y <- y[(y[,'TC'] >= TC_min & y[,'PY']  >= PY_min & y[,'PY'] <= PY_max),]; y <- y[1:n_max,]

    count_before <- nrow(x)
    x %<>% bind_rows(y) %>%
      distinct(UT, .keep_all = TRUE)
    # x <- rbind(x, y); x <- x[!duplicated(x[,"UT"]),]
    count_after <- nrow(x) 
    
    cat("---> Loading dataframe", i, "with", nrow(y), "rows complete.", "Added", (count_after - count_before), "rows\n", sep = " ")   
  }
  
  cat("-> Done! Loading", length(file_list), "dataframes with", nrow(x), "rows complete\n", sep = " ")  
  
  x %<>%
    mutate(XX = XX %>% make.unique(sep='_')) %>%
    as.data.frame()
  rownames(x) <- x$XX
  
  return(x)
}

############################################################################
# Select Seed articels
############################################################################

# Select variables to keep
vars <- c("AU", "Author.s..ID", "TI", "PY", "SO", "VL", "IS", "PP", "TC", "DI", "Affiliations", "C1", "AB", "DE", "ID", "FU", "FX",
          "CR", "RP", "LA", "JI", "DT", "DB", "UT", "J9", "AU_UN", "AU1_UN", "AU_UN_NR", "SR_FULL", "SR")

# Load bibliographic data
file_list <- dir(path = '../input/', pattern = 'scopus_.*\\.csv') 
file_list <- paste0('../input/', file_list)

M <- read_collection(file_list, bib_source = 'scopus', bib_format = 'csv', PY_min = 1990, n_max = 1000) 
#M <- convert2df(file = file_list, dbsource = 'scopus', format = 'csv') 

# Restrict variables
#M <- M[,vars]; rm(vars)

# Extract Meta Tags
M %<>% metaTagExtraction(Field = "AU_CO", aff.disamb = TRUE, sep = ";")

############################################################################
# Overall summary
############################################################################

# CR <- M %>% citations(field = "article", sep = ";")
# CRL<- M %>% localCitations(sep = ";")

############################################################################
# Overall summary
############################################################################
results <- M %>% biblioAnalysis(sep = ";")
results %>% summary(k = 10, pause = FALSE)
results %>% plot(k = 10, pause = FALSE)

############################################################################
# Networks
############################################################################

# Bibliographic
mat <- M %>% biblioNetwork(analysis = "coupling", network = "references", sep = ";")
mat %<>% normalizeSimilarity(type = "association")

############################################################################
# Historical citation
############################################################################

# Create a historical citation network
histResults <- M %>% histNetwork(sep = ";")

histResults %>% histPlot(n = 30, size = 10, labelsize = 5)

############################################################################
# HAdittional fun
############################################################################

M %>% authorProdOverTime(k = 10, graph = TRUE)
M %>% rpys(sep = ";", graph = T)
M %>% thematicMap()
M %>% thematicEvolution(years = c(2000, 2019))
M %>% threeFieldsPlot()

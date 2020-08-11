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
rm(list=ls())

library(tidytext)
text_lda <- readRDS("../temp/text_lda.R")
text_tidy <- readRDS("../temp/text_tidy.R")

terms(text_lda, 10)

topic_term <- text_lda %>% tidy(matrix = "beta")

plot <- topic_term %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(topic = topic %>% factor(),
         term = term %>% reorder_within(beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(legend.position = "none")

plot_ly <- plot %>% plotly::ggplotly()
htmlwidgets::saveWidget(plotly::as_widget(plot_ly), 'output\vis_plotly_topic_terms.html', selfcontained = TRUE)

### LDA Viz
# TTM
text_dtm <- text_tidy %>%
  cast_dtm(document, term, n) %>%
  tm::removeSparseTerms(sparse = .99)

library(LDAvis)
json_lda <- topicmodels_json_ldavis(fitted = text_lda, 
                                    doc_dtm = text_dtm, 
                                    method = "TSNE")


json_lda %>% serVis()
json_lda %>% serVis(out.dir = 'output/LDAviz')


###########################################################################
# HAdittional fun
############################################################################

M %>% authorProdOverTime(k = 10, graph = TRUE)
# M %>% rpys(sep = ";", graph = T)
M %>% thematicMap()
M_them_evo <- M %>% thematicEvolution(years = c(2000, 2019))
M_threefield <- M %>% threeFieldsPlot()


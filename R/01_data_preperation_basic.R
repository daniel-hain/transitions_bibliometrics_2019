############################################################################
# Preamble
############################################################################
### Generic preamble
rm(list=ls())

options(scipen = 5) # To deactivate annoying scientific number notation
set.seed(1337) # To have a seed defined for reproducability

# own functions
source("functions/functions_basic.R")
source("functions/00_parameters.R")

### Load standard packages
library(tidyverse)
library(magrittr)

### Extra packages
library(bibliometrix)
library(tidygraph)

############################################################################
# Select Seed articels
############################################################################

# TODO: INCLUDE THE SEED PAPER!!!!!!!!!!

# Load bibliographic data
file_list <- dir(path = '../input/', pattern = 'scopus_.*\\.csv') 
file_list <- paste0('../input/', file_list)

M <- read_collection(file_list, bib_source = 'scopus', bib_format = 'csv', 
                     TC_min = TC_min,
                     PY_min = PY_min, 
                     n_max = n_max, 
                     filter_DT = 'ARTICLE') 

# Restrict variables
M  %<>% select(-Molecular.Sequence.Numbers, -Chemicals.CAS, -Tradenames, -Manufacturers, -Sponsors, 
               -Conference.name, -Conference.date, -Conference.code, -Conference.location,
               -CODEN, -PubMed.ID, -LA, -Publication.Stage, 
               -starts_with('Funding.Text'))

# Extract Meta Tags #TODO: Maybe more?
M %<>% metaTagExtraction(Field = "AU_CO", aff.disamb = TRUE, sep = ";")

# Save 
M %>% saveRDS("../temp/M.RDS")
rm(file_list, vars, read_collection)

############################################################################
# Networks Bibliographic
############################################################################
# M <- readRDS("../temp/M.RDS")

mat_bib <- M %>% as.data.frame() %>% biblioNetwork(analysis = "coupling", network = "references", sep = ";", shortlabel = FALSE)

g_bib <- mat_bib %>% igraph::graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) %>% 
  igraph::simplify() %>%
  as_tbl_graph(directed = FALSE) # %N>% left_join(M %>% select(XX, SR, PY, TC, J9), by = c("name" = "XX")) %>% mutate(id = 1:n()) 

# Restrict the network
g_bib <- g_bib %E>% 
  filter(weight >= cutof_edge_bib) %N>%
  # filter(percent_rank(weight) >= cutof_edge_pct_bib) %N>%
  filter(!node_is_isolated()) %N>%
  mutate(dgr = centrality_degree(weights = weight)) %N>%
  # filter(dgr >= cutof_node_bib) %>%
  filter(percent_rank(dgr) >= cutof_node_pct_bib)

# # Inspect
# g_bib %N>% mutate(dgr = centrality_degree(weights = weight)) %>% as_tibble() %>% skimr::skim()
# g_bib %E>% as_tibble() %>% skimr::skim()

# Community Detection
g_bib <- g_bib %N>%
  # Level 1
  mutate(com = group_louvain(weights = weight)) %>%
  # Level 2
  morph(to_split, com) %>% 
  mutate(dgr_int = centrality_degree(weights = weight)) %N>%
  mutate(com2 = group_louvain(weights = weight)) %>%
  unmorph() %>%
  morph(to_split, com, com2) %>% 
  mutate(dgr_int2 = centrality_degree(weights = weight)) %>%
  unmorph() 

# Community size restriction
g_bib <- g_bib %N>%
  add_count(com, name = 'com_n') %>%
  add_count(com, com2, name = 'com2_n') %>%
  mutate(com = ifelse(com_n >= com_size_bib, com, NA) ) %>%
  mutate(com2 = ifelse(com2_n >= com2_size_bib, com, NA) ) %>%
  select(-com_n, -com2_n)  

# Delete nodes withou community
g_bib <- g_bib %N>%
  filter(!is.na(com))

# Update degree
g_bib <- g_bib %N>%
  mutate(dgr = centrality_degree(weights = weight))

# Save the objects we need lateron
g_bib %>% saveRDS("../temp/g_bib.RDS")

### Merge with main data
M_bib <- M %>% select(XX) %>% inner_join(g_bib %N>% as_tibble() %>% select(name, dgr, com, dgr_int, com2, dgr_int2), by = c('XX' = 'name')) %>%
  distinct(XX, .keep_all = TRUE) 

# Save and remove
M_bib %>% saveRDS("../temp/M_bib.RDS")

rm(mat_bib, g_bib, com_size_bib, cutof_edge_bib, cutof_node_bib)

### Aggregated Network
require(RNewsflow)
g_bib_agg <- g_bib %>%
  network_aggregate(by = "com", edge_attribute = "weight", agg_FUN = sum)  %>%
  as.undirected(mode = "collapse", edge.attr.comb = "sum") %>%
  as_tbl_graph(directed = FALSE) %N>%
  select(-name) %>%
  mutate(id = 1:n()) %E>%
  rename(weight = agg.weight) %>%
  select(from, to, weight)

# g_bib_agg <- g_bib_agg %E>%
#   rename(weight_count = weight) %>%
#   mutate(weight = weight_count / (.N()$N[from] * .N()$N[to]) ) %>%
#   mutate(weight = (weight * 100) %>% round(4)) %N>%
#   mutate(dgr = centrality_degree(weights = weight))

saveRDS(g_bib_agg, "temp/g_bib_agg.RDS")
rm(g_bib, g_bib_agg)

### Restrict original M
M %<>%
  semi_join(M_bib, by = 'XX')

############################################################################
# Network Cocitation
############################################################################

mat_cit <- M %>%
  as.data.frame() %>% 
  biblioNetwork(analysis = "co-citation", network = "references", sep = ";", shortlabel = FALSE)

# mat_cit %>% saveRDS("../temp/mat_cit.RDS")
# mat_cit <- readRDS("../temp/mat_cit.RDS")

g_cit <- mat_cit %>% igraph::graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) %>% 
  igraph::simplify() %>%
  as_tbl_graph(directed = FALSE) # %N>% left_join(M %>% select(XX, SR, PY, TC, J9), by = c("name" = "XX")) %>% mutate(id = 1:n()) 

# Restrict the network
g_cit <- g_cit %E>% 
  filter(weight >= cutof_edge_cit) %N>%
  # filter(percent_rank(weight) >= cutof_edge_pct_cit) %N>%
  filter(!node_is_isolated()) %N>%
  mutate(dgr = centrality_degree(weights = weight)) %N>%
  filter(dgr >= cutof_node_cit) #%>%
  #filter(percent_rank(dgr) >= cutof_node_pct_cit)

# Weighting # NOTE: Only in Cicit network
g_cit <- g_cit %E>%
  mutate(weight_jac = weight / (.N()$dgr[from] + .N()$dgr[to] - weight) ) %N>%
  mutate(dgr_jac = centrality_degree(weights = weight_jac))

# # Inspect
# g_cit %N>% mutate(dgr = centrality_degree(weights = weight)) %>% as_tibble() %>% skimr::skim()
# g_cit %E>% as_tibble() %>% skimr::skim()

# Community Detection
g_cit <- g_cit %N>%
  mutate(com = group_louvain(weights = weight)) %N>%
  morph(to_split, com) %>% 
  mutate(dgr_int = centrality_degree(weights = weight)) %>%
  unmorph()

# Community size restriction
g_cit <- g_cit %N>%
  add_count(com, name = 'com_n') %>%
  mutate(com = ifelse(com_n >= com_size_cit, com, NA) ) %>%
  select(-com_n)  

# Delete nodes withou community
g_cit <- g_cit %N>%
  filter(!is.na(com))

# Update degree
g_cit <- g_cit %N>%
  mutate(dgr = centrality_degree(weights = weight),
         dgr_jac = centrality_degree(weights = weight_jac))

# Save the objects we need lateron
g_cit %>% saveRDS("../temp/g_cit.RDS")

# generate citation report
C_nw <- g_cit %N>%
  as_tibble()

C_nw %>% saveRDS("../temp/C_nw.RDS")

###A ggregated Network
require(RNewsflow)
g_cit_agg <- g_cit %>%
  network_aggregate(by = "com", edge_attribute = "weight", agg_FUN = sum)  %>%
  as.undirected(mode = "collapse", edge.attr.comb = "sum") %>%
  as_tbl_graph(directed = FALSE) %N>%
  select(-name) %>%
  mutate(id = 1:n()) %E>%
  rename(weight = agg.weight) %>%
  select(from, to, weight)

saveRDS(g_cit_agg, "../temp/g_cit_agg.RDS")
rm(mat_cit, g_cit, g_cit_agg)

#### 2 mode network # TODO
# CRL <- M %>% localCitations(sep = ";") # For some reason takes forever...
# CR <- M %>% citations(field = "article", sep = ";")

m_2m <- M %>% cocMatrix(Field = "CR", sep = ";")

g_2m <- m_2m %>% igraph::graph_from_incidence_matrix(directed = TRUE, weighted = FALSE) %>% 
  igraph::simplify() 

el_2m <- g_2m %>%
  get.edgelist() %>%
  as_tibble() %>%
  rename(from = V1,
         to = V2)

el_2m %<>%
  inner_join(M_bib %>% select(XX, com), by = c('from' = 'XX')) %>%
  rename(com_bib = com) %>%
  inner_join(C_nw %>% select(name, com), by = c('to' = 'name')) %>%
  rename(com_cit = com)

saveRDS(el_2m, "../temp/el_2m.RDS")
rm(m_2m, g_2m, el_2m)

############################################################################
# Overall summary
############################################################################
results <- M %>% biblioAnalysis(sep = ";")

# Save 
results %>% saveRDS("../temp/results.RDS")
rm(results)
############################################################################
# Historical citation
############################################################################

# Create a historical citation network
histResults <- M %>% histNetwork(sep = ";")
histResults %>% saveRDS("../temp/histResults.RDS")
histResults %>% histPlot(n = 50, size = 10, labelsize = 5)

rm(histResults)

############################################################################
# Conceptual Structure
############################################################################

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
CS %>% saveRDS("../temp/CS.RDS")
rm(CS)


############################################################################
# Topicmodel
############################################################################

library(tidytext)
library(topicmodels)
library(textstem)

# Extract text to work with
text_tidy <- M %>% 
  as_tibble() %>%
  select(XX, AB) %>%
  rename(document = XX) 

# Some initial cleaning
text_tidy %<>% 
  mutate(AB = AB %>% 
           str_replace_all("&", "-and-") %>%
           str_remove_all("/(&trade;|&reg;|&copy;|&#8482;|&#174;|&#169;)/.*") %>%
           str_squish() %>%
           iconv(to = "UTF-8", sub = "byte") %>%
           str_remove("�.*") 
  )  %>%
  drop_na() 

# Unnesting
text_tidy %<>% 
  unnest_tokens(term, AB) 

# filtering
text_tidy %<>%
  filter(str_length(term) > 2)

# Stopwords
stopwords_own <- tibble(
  word =c("study", "paper", "result", "model", "approach", "article", "author", "method", "understand", "focus", "examine", "aim", "argue", "identify",
                   "increase", "datum", "potential", "explore", "include", "issue", "propose", "address", "apply", "require", "analyse", "relate", "finding",
                   "analyze", "discuss", "contribute", "publish", "involve", "draw", "lead", "exist", "set", "reduce", "create", "form", "explain", "play",
                   "affect", "regard", "associate", "establish", "follow", "conclude", "define", "strong", "attempt", "finally", "elsevier", "offer",
                   "taylor", "francis", "copyright", "springer", "wiley", "emerald", "copyright"))

text_tidy %<>%
  anti_join(stop_words, by = c('term' = 'word')) %>%
  anti_join(stopwords_own, by = c('term' = 'word')) %>%
  count(document, term, sort = TRUE) 
rm(stopwords_own)

# Lemmatizing 
lemma_own <- tibble( # WORK IN THAT !!!!!!!!!!
  token = c("institutional", "technological", "national", "regional", "sustainable",    "environmental", "political"),
  lemma = c("institution",   "technology",    "nation",   "region",   "sustainability", "environment",   "policy"))

text_tidy %<>%
  mutate(term = term %>% lemmatize_words(dictionary = lexicon::hash_lemmas %>% filter(token != 'data') %>%
                                           anti_join(lemma_own, by = 'token') %>%
                                           bind_rows(lemma_own) ))
text_tidy %>% saveRDS("../temp/text_tidy.R")
rm(lemma_own)

# TTM
text_dtm <- text_tidy %>%
  cast_dtm(document, term, n) %>%
  tm::removeSparseTerms(sparse = .99)

# LDA
text_lda <- text_dtm %>% LDA(k = 6, control = list(seed = 1337))
text_lda %>% saveRDS("../temp/text_lda.R")

rm(text_tidy, text_dtm, text_lda)


############################################################################
# Other network levels
############################################################################
# mat_bib %<>% normalizeSimilarity(type = "association") # NOTE: We do not normalize on the biblio-network publication level anymore.


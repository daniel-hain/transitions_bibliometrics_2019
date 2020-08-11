############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

# Extra functions
source("functions/functions_general.R")
source("functions/functions_nw.R")

# Extra packages
library(Matrix)
require(igraph)
require(tidygraph)

set.seed(1337)
# TODO
# Check: require(disparityfilter) # g <-  get.backbone(graph=g, alpha=0.05, directed=F)

#####################################################################################################
# Do the matrix
#####################################################################################################

select <- readRDS("temp/M_comleted.RDS") %>% pull(EID) %>% unique()
cit_el <- readRDS("temp/cit_el.RDS") %>% select(EID, CR_SID)

cit_el %<>%
  filter(EID %in% select) %>%
  rename(art = EID,
         cit = CR_SID) %>%
  drop_na() %>%
  distinct(art, cit)

# Generate a sparse matrix # TODO: If we want to weight something related to the occurence matrix, we can do it now on our own here. Also: Make clean colnames before matrix
mat <- spMatrix(nrow=length(unique(cit_el$art)),
                ncol=length(unique(cit_el$cit)),
                i = as.numeric(factor(cit_el$art)),
                j = as.numeric(factor(cit_el$cit)),
                x = rep(1, length(as.numeric(cit_el$art))) ) # TODO: Check why there are NAs
row.names(mat) <- levels(factor(cit_el$art))
colnames(mat) <- levels(factor(cit_el$cit))

mat.art <- tcrossprod(mat)
mat.cit <- tcrossprod(t(mat))

saveRDS(mat.art, "temp/mat_art.RDS")
saveRDS(mat.cit, "temp/mat_cit.RDS")

#####################################################################################################
# Bibliographic Coupling network
#####################################################################################################

rm(list=ls())
source("functions/parameters.R")
source("functions/functions_general.R")
source("functions/functions_nw.R")

M <- readRDS("temp/M_comleted.RDS")
mat <- readRDS("temp/mat_art.RDS")

### Convert to igraph
g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE) %>% 
  as_tbl_graph(directed = FALSE)  %N>%
  left_join(M %>% select(EID, SR, PY, NR, TC, DT), by = c("name" = "EID")) %>%
  mutate(id = 1:n())%E>% 
  filter(weight > 0 & from != to)
rm(mat)

# g <- g %E>% 
#   mutate(from_EID = .N()$name[from],
#          to_EID = .N()$name[to]) 

### Weighting the edges: JACCARD v# NOte: Alternative, to consider
g <- g %E>%
  mutate(weight.count = weight) %>%
  mutate(weight = weight.count / (.N()$NR[from] + .N()$NR[to] - weight.count) ) %>%
  mutate(weight = weight %>% round(3)) 

# left_join((. %N>% as_tibble() %>% select(id, NR) %>% rename(NR_from = NR)), by = c("from" = "id")  ) %>%
# left_join((. %N>% as_tibble() %>% select(id, NR) %>% rename(NR_to = NR)), by = c("to" = "id")  ) %>%  


### Reduce the graph
g <- g %E>%
  filter(weight > 0) %>%  
  filter(weight > quantile(weight, cutof.bib, na.rm = TRUE))  %N>%
  filter(centrality_degree(weights = weight) > 0) %>%
  filter(centrality_degree(weights = weight) > quantile(centrality_degree(weights = weight), cutof.bib, na.rm = TRUE) )

# g %E>% pull(weight) %>% skim()
# g %N>% mutate(dgr = centrality_degree(weights = weight)) %>% pull(dgr) %>% skim()
# g %N>% mutate(dgr = centrality_degree(weights = weight.count)) %>% pull(dgr) %>% skim()

### community detection

# 1st level
g <- g %N>%
  mutate(com = group_louvain(weights = weight)) %>%
  group_by(com) %>%
  mutate(com_n = n()) %>%
  ungroup() %>%
  filter(com_n >= res.bib) %>%
  select(-com_n)   %>%
  mutate(dgr = centrality_degree(weights = weight))

#2nd level
g <- g %N>%
  morph(to_split, com) %>% 
  mutate(com2 = group_louvain(weights = weight)) %>%
  unmorph() %>%
  group_by(com2) %>%
  mutate(com_n = n()) %>%
  ungroup() %>%
  mutate(com2 = ifelse(com_n >= 50, com2, NA) ) %>%
  select(-com_n)  

# internal degree
g <- g %N>%
  morph(to_split, com) %>% 
  mutate(dgr_int = centrality_degree(weights = weight)) %>%
  unmorph() %>%
  morph(to_split, com2) %>% 
  mutate(dgr_int2 = centrality_degree(weights = weight)) %>%
  unmorph()

# Save the objects we need lateron
saveRDS(g, "temp/g_bib.RDS")

### Merge with main data
merge <- g %N>%
  as_tibble() %>%
  rename(EID = name) %>%
  select(EID, dgr, com, dgr_int, com2, dgr_int2)

M %<>% select(EID) %>% left_join(merge, by = "EID") %>%
  distinct(EID, .keep_all = TRUE) 

saveRDS(M, "temp/M_nw.RDS")

#####################################################################################################
### Aggregated
#####################################################################################################

# NEeds some check up
require(RNewsflow)
g.agg <- g %>%
  network.aggregate(by = "com", edge.attribute = "weight", agg.FUN = sum)  %>%
  as.undirected(mode = "collapse", edge.attr.comb = "sum") %>%
  as_tbl_graph(directed = FALSE) %N>%
  select(-name) %>%
  mutate(id = 1:n()) %E>%
  rename(weight = agg.weight) %>%
  select(from, to, weight)

g.agg <- g.agg %E>%
  rename(weight_count = weight) %>%
  mutate(weight = weight_count / (.N()$N[from] * .N()$N[to]) ) %>%
  mutate(weight = (weight * 100) %>% round(4)) %N>%
  mutate(dgr = centrality_degree(weights = weight))

saveRDS(g.agg, "temp/g_bib_agg.RDS")

#####################################################################################################
# Co-Citation network
#####################################################################################################

rm(list=ls())
source("functions/parameters.R")
source("functions/functions_general.R")
source("functions/functions_nw.R")

mat <- readRDS("temp/mat_cit.RDS")
C <- readRDS("temp/C_data.RDS") 

### Convert to igraph
g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)  %>% 
  as_tbl_graph(directed = FALSE) %N>%
  left_join(C %>% select(SID, SR, PY, TC), by = c("name" = "SID")) %>%
  mutate(id = 1:n()) %E>% 
  filter(from != to)

# ### Weighting the edges: No jaccard, only normalization
g <- g %E>%
  mutate(weight.count = weight) %>%
  mutate(weight =  norm_01(weight.count, na.rm = TRUE) %>% round(3) ) %>%
  filter(weight > 0)

### Reduce the graph
g <- g %E>%
  filter(weight > 0) %>%  
  filter(weight > quantile(weight, cutof.cit, na.rm = TRUE))  %N>%
  filter(centrality_degree(weights = weight) > 0) %>%
  filter(centrality_degree(weights = weight) > quantile(centrality_degree(weights = weight), cutof.cit, na.rm = TRUE) )

### community detection
g <- g %N>%
  mutate(com = group_louvain(weights = weight)) %>%
  group_by(com) %>%
  mutate(com_n = n()) %>%
  ungroup() %>%
  filter(com_n >= res.cit) %>%
  select(-com_n)

# internal degree
g <- g %N>%
  mutate(dgr = centrality_degree(weights = weight)) %>%
  morph(to_split, com) %>% 
  mutate(dgr_int = centrality_degree(weights = weight)) %>%
  unmorph()
  
# Save the objects we need lateron
saveRDS(g, "temp/g_cit.RDS")


### Merge with main data
merge <- g %N>%
  as_tibble() %>%
  rename(SID = name) %>%
  select(SID, com, dgr, dgr_int)

C %<>% inner_join(merge, by = "SID") %>%
  distinct(SID, .keep_all = TRUE) 

saveRDS(C, file="temp/C_nw.RDS")

#####################################################################################################
### Aggregated
#####################################################################################################

g <- readRDS("temp/g_cit.RDS")

# Com level
require(RNewsflow)
g.agg <- g %>%
  network.aggregate(by = "com", edge.attribute = "weight", agg.FUN = sum)  %>%
  as.undirected(mode = "collapse", edge.attr.comb = "sum") %>%
  as_tbl_graph() %N>%
  select(-name) %>%
  mutate(id = 1:n())%E>%
  rename(weight = agg.weight) %>%
  select(from, to, weight)

g.agg <- g.agg %E>%
  left_join((. %N>% as_tibble() %>% rename(N_from = N)), by = c("from" = "id")  ) %>%
  left_join((. %N>% as_tibble() %>% rename(N_to = N)), by = c("to" = "id")  ) %>%  
  rename(weight_count = weight) %>%
  mutate(weight = weight_count / (N_from * N_to) ) %>%
  mutate(weight = (weight * 100) %>% round(4)) %N>%
  mutate(dgr = centrality_degree(weights = weight))

saveRDS(g.agg, "temp/g_cit_agg.RDS")


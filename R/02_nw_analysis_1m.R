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
require(ggraph)
require(ggiraph)

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
g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)  %>% 
  as_tbl_graph() %E>% 
  filter(weight > 0 & from != to) %N>%
  left_join(M %>% select(EID, SR, PY, NR, TC, DT), by = c("name" = "EID"))

### Weighting the edges: JACCARD v# NOte: Alternative, to consider
g <- g %E>%
  mutate(weight.count = weight) %>%
  left_join((. %N>% as_tibble() %>% rename(NR_from = NR)), by = c("from" = "name")  ) %>%
  left_join((. %N>% as_tibble() %>% rename(NR_to = NR)), by = c("to" = "name")  ) %>%  
  mutate(weight = weight.count / (NR_from + NR_to - weight.count) ) %>%
  mutate(weight = weight %>% round(3))

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

M %<>% inner_join(merge, by = "EID") %>%
  distinct(EID, .keep_all = TRUE) 

saveRDS(M, "temp/M_nw.RDS")



### visual exploration
rm(list=ls())
g <- readRDS("temp/g_bib.RDS")

g.com <- g %N>%
  filter(TC >= 5 & NR >= 5) %>%
  filter( !(DT %in% c("Review", "Conference Paper")) ) %>% 
  filter(com == 2) %>%
  arrange(desc(dgr_int)) %>% 
  slice(1:50) %E>%
  filter(weight >= quantile(weight, 0.75) ) %N>% 
  filter(centrality_degree(weights = weight) > 0)

g.com  %>%
  ggraph(layout = "fr") + 
#  geom_edge_density(aes(fill = weight)) +
  geom_edge_arc(curvature = 0.1, aes(width = weight), alpha = 0.2)  + 
  geom_node_point(aes(colour = factor(com2), size = dgr_int)  )  + 
  geom_node_text(aes(label = SR), repel = TRUE) +
  scale_color_brewer(palette = "Set1") + 
  theme_graph() 

# Com level
require(RNewsflow)
g.agg <- g %>%
  network.aggregate(by = "com", edge.attribute = "weight", agg.FUN = sum)  %>%
  as.undirected(mode = "collapse", edge.attr.comb = "sum") %>%
  as_tbl_graph() %N>%
  select(-name) %E>%
  rename(weight = agg.weight) %>%
  select(from, to, weight)

g.agg <- g.agg %E>%
  left_join((. %N>% as_tibble() %>% rename(N_from = N)), by = c("from" = "com")  ) %>%
  left_join((. %N>% as_tibble() %>% rename(N_to = N)), by = c("to" = "com")  ) %>%  
  rename(weight_count = weight) %>%
  mutate(weight = weight_count / (N_from * N_to) ) %>%
  mutate(weight = (weight * 100) %>% round(4)) %N>%
  mutate(dgr = centrality_degree(weights = weight))

g.agg %E>% 
  filter(weight > 0 & from != to) %>%
  filter(weight >= quantile(weight, 0.25) )  %>%
  ggraph(layout = "circle") + 
  geom_edge_arc(curvature = 0.075, aes(width = weight), alpha = 0.2)  + 
  geom_node_point(aes(size = N, color = dgr)  )  + 
  geom_node_text(aes(label = as.character(com)), repel = TRUE) +
  theme_graph() 

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
  as_tbl_graph() %E>% 
  filter(weight > 0 & from != to) %N>%
  left_join(C %>% select(SID, SR, PY, TC), by = c("name" = "SID"))

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

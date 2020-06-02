############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

# Extra functions
source("functions/functions_general.R")
source("functions/functions_nw.R")

# Extra packages
library(Matrix)
require(tidygraph)
require(ggraph)

set.seed(1337)

#####################################################################################################
#####################################################################################################
#####################################################################################################
### Bibliographic coupling
#####################################################################################################
#####################################################################################################
#####################################################################################################
M <- readRDS("temp/M_nlp.RDS")
C = readRDS("temp/C_nw.RDS")
el_topic <- readRDS("temp/el_topic.RDS")


g <- readRDS("temp/g_bib.RDS")

### Topic aggregate




edges_agg <- el_topic %>%
  rename(from = com,
         to = topic,
         weight_count = gamma) %>%
  group_by(from, to) %>% summarize(weight_count = sum(weight_count)) %>% ungroup() %>%
  mutate(weight = weight_jaccard(x = ., i = "from", j = "to", w = "weight_count")) %>%
  select(from, to, weight)
  
g.2mt <- as_tbl_graph(edges_agg, directed = TRUE, mode = "out")

g.2mt %>%
  ggraph(layout = 'fr') +
  geom_edge_arc(curvature = 0.1, aes(width = weight), alpha = 0.1)  + 
  geom_node_point( )  +
  theme_graph(base_family = "Arial")




















#####################################################################################################
##### Articles
#####################################################################################################
rm(list=ls())
source("00_functions.R")
source("00_parameters.R")

M <- readRDS("temp/M_final.RDS")
names <- readRDS("temp/names_art.RDS")

# year
com <- expand.grid(com = unique(names), PY = unique(M$PY), stringsAsFactors = F) %>% 
  left_join(M %>% group_by(com, PY) %>% 
              summarise(n = n(), TC = sum(TC), NR = sum(NR), dgr = sum(dgr), dgr.int = sum(dgr.int)) %>% ungroup()
            , by = c("com", "PY")) %>% distinct(com, PY, .keep_all = T) %>%
  mutate(dgr.ext = dgr - dgr.int) %>%
  arrange(com, PY) %>%
  replace_na(list(n = 0, TC = 0, NR = 0, dgr = 0, dgr.int = 0, dgr.ext = 0)) %>%
  mutate(dgr = round(dgr, 2), dgr.int = round(dgr.int, 2), dgr.ext = round(dgr.ext, 2)) %>%
  mutate(n.cum = ave(.$n, .$com, FUN = cumsum), 
         TC.cum = ave(.$TC, .$com, FUN = cumsum),
         NR.cum = ave(.$NR, .$com, FUN = cumsum),
         dgr.cum = ave(.$dgr, .$com, FUN = cumsum),
         dgr.int.cum = ave(.$dgr.int, .$com, FUN = cumsum),
         dgr.ext.cum = ave(.$dgr.ext, .$com, FUN = cumsum)) %>%
  mutate(dgr.ratio = round(dgr.int.cum / dgr.cum, 2)) %>%
  filter(PY >= 1980 & PY <= 2018) %>% 
  mutate(P = NA) %>%
  arrange(com, PY) 
  
for (i in length(time):1) {com[com$PY < time[i],"P"] <- i}  
com[is.infinite(com$dgr.ratio) | is.nan(com$dgr.ratio), "dgr.ratio"] <- 0 

# period
com.p <- expand.grid(com = unique(names), P = unique(M$P), stringsAsFactors = F) %>% 
  left_join(M %>% group_by(com, P) %>% 
              summarise(n = n(), TC = sum(TC), NR = sum(NR), dgr = sum(dgr), dgr.int = sum(dgr.int)) %>% ungroup()
            , by = c("com", "P")) %>% distinct(com, P, .keep_all = T) %>%
  mutate(dgr.ext = dgr - dgr.int) %>%
  arrange(com, P) %>%
  replace_na(list(n = 0, TC = 0, NR = 0, dgr = 0, dgr.int = 0, dgr.ext = 0)) %>%
  mutate(dgr = round(dgr, 2), dgr.int = round(dgr.int, 2), dgr.ext = round(dgr.ext, 2)) %>%
  mutate(n.cum = ave(.$n, .$com, FUN = cumsum), 
         TC.cum = ave(.$TC, .$com, FUN = cumsum),
         NR.cum = ave(.$NR, .$com, FUN = cumsum),
         dgr.cum = ave(.$dgr, .$com, FUN = cumsum),
         dgr.int.cum = ave(.$dgr.int, .$com, FUN = cumsum),
         dgr.ext.cum = ave(.$dgr.ext, .$com, FUN = cumsum)) %>%
  mutate(dgr.ratio = round(dgr.int.cum / dgr.cum, 2)) %>%
  arrange(com, P) 
com.p[is.infinite(com.p$dgr.ratio) | is.nan(com.p$dgr.ratio), "dgr.ratio"] <- 0 

# save 
saveRDS(com, "temp/art_com_y.RDS")
saveRDS(com.p, "temp/art_com_p.RDS")

#####################################################################################################
##### Citations
#####################################################################################################
rm(list=ls())
source("00_functions.R")
source("00_parameters.R")

C <- readRDS("temp/C_final.RDS") %>% mutate(PY = as.numeric(PY))
names <- readRDS("temp/names_cit.RDS")

# year
com <- expand.grid(com = unique(names), PY = unique(C$PY), stringsAsFactors = F) %>% 
  left_join(C %>% group_by(com, PY) %>% 
              summarise(n = n(), TC = sum(TC), dgr = sum(dgr), dgr.int = sum(dgr.int)) %>% ungroup()
            , by = c("com", "PY")) %>% distinct(com, PY, .keep_all = T) %>%
  mutate(dgr.ext = dgr - dgr.int) %>%
  arrange(com, PY)  %>%
  replace_na(list(n = 0, TC = 0, dgr = 0, dgr.int = 0, dgr.ext = 0)) %>%
  mutate(dgr = round(dgr, 2), dgr.int = round(dgr.int, 2), dgr.ext = round(dgr.ext, 2)) %>%
  mutate(n.cum = ave(.$n, .$com, FUN = cumsum), 
         TC.cum = ave(.$TC, .$com, FUN = cumsum),
         dgr.cum = ave(.$dgr, .$com, FUN = cumsum),
         dgr.int.cum = ave(.$dgr.int, .$com, FUN = cumsum),
         dgr.ext.cum = ave(.$dgr.ext, .$com, FUN = cumsum)) %>%
  mutate(dgr.ratio = round(dgr.int.cum / dgr.cum, 2)) %>%
  filter(PY >= 1980 & PY <= 2018) %>% 
  mutate(P = NA)  %>%
  arrange(com, PY) 

for (i in length(time):1) {com[com$PY < time[i],"P"] <- i}  
com[is.infinite(com$dgr.ratio)| is.nan(com$dgr.ratio), "dgr.ratio"] <- 0 

# period
com.p <- expand.grid(com = unique(names), P = unique(C$P), stringsAsFactors = F) %>% 
  left_join(C %>% group_by(com, P) %>% 
              summarise(n = n(), TC = sum(TC), dgr = sum(dgr), dgr.int = sum(dgr.int)) %>% ungroup()
            , by = c("com", "P")) %>% distinct(com, P, .keep_all = T) %>%
  mutate(dgr.ext = dgr - dgr.int) %>%
  arrange(com, P)  %>%
  replace_na(list(n = 0, TC = 0, dgr = 0, dgr.int = 0, dgr.ext = 0)) %>%
  mutate(dgr = round(dgr, 2), dgr.int = round(dgr.int, 2), dgr.ext = round(dgr.ext, 2)) %>%
  mutate(n.cum = ave(.$n, .$com, FUN = cumsum), 
         TC.cum = ave(.$TC, .$com, FUN = cumsum),
         dgr.cum = ave(.$dgr, .$com, FUN = cumsum),
         dgr.int.cum = ave(.$dgr.int, .$com, FUN = cumsum),
         dgr.ext.cum = ave(.$dgr.ext, .$com, FUN = cumsum)) %>%
  mutate(dgr.ratio = round(dgr.int.cum / dgr.cum, 2)) %>%
  arrange(com, P) 
com.p[is.infinite(com.p$dgr.ratio)| is.nan(com.p$dgr.ratio), "dgr.ratio"] <- 0 

# save 
saveRDS(com, "temp/cit_com_y.RDS")
saveRDS(com.p, "temp/cit_com_p.RDS")

#####################################################################################################
#####################################################################################################
#####################################################################################################
### Networks
#####################################################################################################
#####################################################################################################
#####################################################################################################

#####################################################################################################
#################### 1 mode: bibliographic coupling communities
#####################################################################################################

rm(list=ls())
source("00_functions.R")
source("00_parameters.R")
M <- readRDS("temp/M_final.RDS")
g <- readRDS("temp/g_bib_final.RDS")
names <- readRDS("temp/names_art.RDS")

### Create communities directly from the rgaph edgelist
com.p <-  readRDS("temp/art_com_p.RDS") %>%  mutate(n.rel = n / max(n), NR.rel = NR / max(NR))

el <- as_data_frame(g, what = "edges") %>%
  left_join(M %>% select(SR, com, PY), by = c("from" = "SR")) %>% rename(com1 = com, PY1 = PY) %>%
  left_join(M %>% select(SR, com, PY), by = c("to" = "SR")) %>% rename(com2 = com, PY2 = PY) %>%
  distinct(from, to, .keep_all = T) %>%
  mutate(P1 = NA, P2 = NA)
for (i in length(time):1) {el[el$PY1 < time[i],"P1"] <- i;   el[el$PY2 < time[i],"P2"] <- i}  

### NW By periods
el.p.temp <- el %>%
  filter(P1 == P2) %>% rename(P = P1) %>%
  group_by(com1, com2, P) %>% summarise(weight = sum(weight), weight.count = sum(weight.count)) %>% ungroup() %>%
  left_join(com.p %>% select(com, P, n, NR), by = c("com1" = "com", "P")) %>% rename(n1 = n, NR1 = NR) %>%
  left_join(com.p %>% select(com, P, n, NR), by = c("com2" = "com", "P")) %>% rename(n2 = n, NR2 = NR) %>%
  distinct(com1, com2, P, .keep_all = T) %>%
  filter(com1!=com2) %>%
  mutate(weight.adj = weight /  (n1 * n2)  ) %>%
  select(com1, com2, P, weight, weight.adj) 

### Graph by period
g.all <- graph.empty(n=length(names), directed=F) # Whole graph
V(g.all)$name <- names; rm(names)

g.p <- list()
el.p <- data.frame()
for (i in seq(1, length(time) + 1, 1)) { # i=1
  # Note: We not take the degree as nodesize, so dont need community size (and therefore object s) anymore
  s <- com.p %>% rename(name = com, size = n.rel) %>% arrange(name) 
  if(i > length(time)) { s %<>%  mutate(size = n.cum / max(n.cum)) %>% filter(P == i-1) %>% select(name, size)
  } else{s %<>% filter(P == i) %>% select(name, size) }
  e <- el.p.temp 
  if(i > length(time)) { e %<>%  group_by(com1, com2) %>% summarise(weight = sum(weight.adj)) %>% ungroup() %>% 
      mutate(weight = weight / max(weight)) %>% as.matrix()
  } else{e %<>% mutate(weight = weight.adj / max(weight.adj)) %>% filter(P == i) %>% select(com1, com2, weight) %>% as.matrix()}
  
  g <- graph_from_edgelist(e[,1:2], directed = F) 
  E(g)$weight <- as.numeric(e[,3])
  g <- simplify(g, remove.loops = T, edge.attr.comb = "max") 
  g <- g.all + g
  V(g)$size <- as.numeric((strength(g) / max(strength(g))) )
  V(g)[is.na(V(g)$size)]$size <- 0
  #g <- graph_attr_match(g, s, "name", c('name',"size"))
  g.p[[i]] <- g
  
  x <- as_data_frame(g, what = "edges")
  colnames(x) <- c("com1", "com2", "weight")
  x$P <- i
  el.p %<>% bind_rows(x) 
  rm(s, e, g, x)
}

saveRDS(el, "temp/el_bib.RDS")
saveRDS(el.p, "temp/el_bib_p.RDS")
saveRDS(g.p, "temp/g_bib_p.RDS")

#####################################################################################################
#################### 1 mode: Co-citation communities
#####################################################################################################

rm(list=ls())
source("00_functions.R")
source("00_parameters.R")
C <- readRDS("temp/C_final.RDS")
g <- readRDS("temp/g_cit_final.RDS")
names <- readRDS("temp/names_cit.RDS")

### Create communities directly from the rgaph edgelist
com.p <-  readRDS("temp/cit_com_p.RDS") %>%  mutate(n.rel = n / max(n), TC.rel = TC / max(TC))

el <- as_data_frame(g, what = "edges") %>%
  left_join(C %>% select(cit, com, PY), by = c("from" = "cit")) %>% rename(com1 = com, PY1 = PY) %>%
  left_join(C %>% select(cit, com, PY), by = c("to" = "cit")) %>% rename(com2 = com, PY2 = PY) %>%
  distinct(from, to, .keep_all = T) %>%
  mutate(P1 = NA, P2 = NA) %>%
  filter(!is.na(PY1) & !is.na(PY2) )
for (i in length(time):1) {el[el$PY1 < time[i],"P1"] <- i;   el[el$PY2 < time[i],"P2"] <- i}  

### NW By periods
el.p.temp <- el %>%
  filter(P1 == P2) %>% rename(P = P1) %>%
  group_by(com1, com2, P) %>% summarise(weight = sum(weight)) %>% ungroup() %>%
  distinct(com1, com2, P, .keep_all = T) %>%
  filter(com1!=com2) %>%
  group_by(com1, P) %>% mutate(dgr1 = sum(weight)) %>% ungroup() %>%
  group_by(com2, P) %>% mutate(dgr2 = sum(weight)) %>% ungroup() %>%
  mutate(weight.adj = weight /  (dgr1 + dgr2 - weight) ) %>% # JACCARD weight
  select(com1, com2, P, weight, weight.adj) 

### Graph by period
g.all <- graph.empty( n = length(names), directed = F) # Whole graph
V(g.all)$name <- names; rm(names)

g.p <- list()
el.p <- data.frame()
for (i in seq(1, length(time) + 1, 1)) { # i=5
  s <- com.p %>% rename(name = com, size = n.rel) %>% arrange(name) 
  if(i > length(time)) { s %<>%  mutate(size = n.cum / max(n.cum)) %>% filter(P == i-1) %>% select(name, size)
  } else{s %<>% filter(P == i) %>% select(name, size) }
  e <- el.p.temp 
  if(i > length(time)) { e %<>%  group_by(com1, com2) %>% summarise(weight = sum(weight.adj)) %>% ungroup() %>% 
      mutate(weight = weight / max(weight)) %>% as.matrix()
  } else{e %<>% mutate(weight = weight.adj / max(weight.adj)) %>% filter(P == i) %>% select(com1, com2, weight) %>% as.matrix()}
  
  g <- graph_from_edgelist(e[,1:2], directed = F) 
  E(g)$weight <- as.numeric(e[,3])
  g <- delete.edges(g, E(g)[E(g)$weight == 0]) 
  g <- simplify(g, remove.loops = T, edge.attr.comb = "max") # TODO: Maybe mean ???
  g <- g.all + g
  V(g)$size <- as.numeric((strength(g) / max(strength(g))) )
  V(g)[is.na(V(g)$size)]$size <- 0
  #g <- graph_attr_match(g, s, "name", c('name',"size"))
  g.p[[i]] <- g
  
  x <- as_data_frame(g, what = "edges")
  colnames(x) <- c("com1", "com2", "weight")
  x$P <- i
  el.p %<>% bind_rows(x)
  rm(s, e, g, x)
}

saveRDS(el, "temp/el_cit.RDS")
saveRDS(el.p, "temp/el_cit_p.RDS")
saveRDS(g.p, "temp/g_cit_p.RDS")

#####################################################################################################
#################### 1 mode: Topic communities
#####################################################################################################

# STill to do. Needs first an topic NW, i guess
# STill to do. Needs first an topic NW, i guess
# STill to do. Needs first an topic NW, i guess
# STill to do. Needs first an topic NW, i guess
# STill to do. Needs first an topic NW, i guess


#####################################################################################################
#################### 2 mode: Co-citation + Bibliographic communities
#####################################################################################################

rm(list=ls())
source("00_functions.R")
source("00_parameters.R")

cit.el <- readRDS("temp/cit_el_final.RDS")
art.com <- readRDS("temp/art_com_p.RDS")
cit.com <- readRDS("temp/cit_com_p.RDS")

com.2m.p <- bind_rows(art.com %>% mutate(n.rel = n / max(n)) %>% select(com, P, n.rel),
                 cit.com %>% mutate(n.rel = n / max(n)) %>% select(com, P, n.rel))

cit.el %<>%
  group_by(art) %>% mutate(art.dgr = n()) %>% ungroup %>%
  group_by(cit) %>% mutate(cit.dgr = n()) %>% ungroup %>%
  mutate(weight = 1 / (art.dgr + cit.dgr -1) ) # Dont know if that makes sense
  
### 2 mode edgelist on com / year level 
el <- expand.grid(com.art = unique(cit.el$com.art), com.cit = unique(cit.el$com.cit), PY = unique(cit.el$PY.art), stringsAsFactors = F) %>% 
  left_join(cit.el %>% group_by(com.art, com.cit, PY.art) %>% summarise(weight.count = n()) %>% ungroup(),
            by = c("com.art", "com.cit", "PY" = "PY.art"))%>% distinct(com.art, com.cit, PY, .keep_all = T) %>%
  arrange(com.art, com.cit, PY) %>%
  replace_na(list(weight.count = 0)) %>%
  group_by(com.art, PY) %>% mutate(art.dgr = sum(weight.count)) %>% ungroup() %>%
  group_by(com.cit, PY) %>% mutate(cit.dgr = sum(weight.count)) %>% ungroup() %>%
  mutate(weight = weight.count / (art.dgr + cit.dgr - weight.count)) %>% # Jaccard weights
  select(com.art, com.cit, PY, weight, weight.count) %>%
  filter(PY >=1980 & PY <= 2018) %>% 
  mutate(P = NA)  
for (i in length(time):1) {el[el$PY < time[i],"P"] <- i }  
el[is.infinite(el$weight) | is.nan(el$weight) | is.na(el$weight), "weight"] <- 0 

### NW By periods
el.p <- el %>%
  rename(com1 = com.art, com2 = com.cit) %>%
  group_by(com1, com2, P) %>% summarise(weight = sum(weight), weight.count = sum(weight.count)) %>% ungroup() %>%
  group_by(com1, P) %>% mutate(dgr1 = sum(weight.count)) %>% ungroup() %>%
  group_by(com2, P) %>% mutate(dgr2 = sum(weight.count)) %>% ungroup() %>%
  mutate(weight.adj = weight.count /  (dgr1 + dgr2 - weight.count) ) %>% # JACCARD weight
  select(com1, com2, P, weight, weight.count, weight.adj) 

### Graph by period
names <- bind_rows(art.com %>% select(com), cit.com %>% select(com))%>% arrange(com) %>% distinct(com) %>% pull()
g.all <- graph.empty(n = length(names), directed = T) # Whole graph
V(g.all)$name <- names; rm(names)
V(g.all)$type <- substr(V(g.all)$name, 1,3) 

g.p <- list()
for (i in 1:(length(time) + 1)) { # i=1
  s <- com.2m.p %>% rename(name = com, size = n.rel) %>% arrange(name) %>% filter(P == i) %>% select(name, size)
  e <- el.p %>%  mutate(weight = weight.adj / max(weight.adj))
  if(i < length(time) + 1) {e %<>% filter(P == i)}
  e %<>% select(com1, com2, weight) %>% as.matrix()
  g <- graph_from_edgelist(e[,1:2], directed = T) 
  E(g)$weight <- as.numeric(e[,3])
  g <- simplify(g, remove.loops = T, edge.attr.comb = "max") 
  g <- g.all + g
  #g <- graph_attr_match(g, s, "name", c('name',"size"))
  V(g)$size <- as.numeric((strength(g) / max(strength(g))) )
  V(g)[is.na(V(g)$size)]$size <- 0
  
  g.p[[i]] <- g
  rm(s,e,g)
}

# Save 
saveRDS(el, "temp/el_2m_cit_y.RDS")
saveRDS(el.p, "temp/el_2m_cit_p.RDS")
saveRDS(g.p, "temp/g_2m_cit_p.RDS")

#####################################################################################################
#################### 2 mode: Topics + Bibliographic communities
#####################################################################################################

rm(list=ls())
source("00_functions.R")
source("00_parameters.R")

art.com <- readRDS("temp/art_com_p.RDS")
t <- readRDS("temp/lda_results_gamma.RDS")
M <- readRDS("temp/M_final.RDS")

colnames(t) <- c("SR", sprintf("TOP%02d", 1:(ncol(t)-1)) ) 

t %<>%
  inner_join(M %>% select(SR, com, PY, P), by = "SR") %>% distinct(SR, .keep_all = T) %>%
  select(SR, com, PY, P, everything()) 

### make long
el <- t %>% 
  gather(topic, weight, 5:ncol(.), -PY, -com) %>%
  arrange(SR, topic)

### aggregate for period
el.p <- expand.grid(com = unique(M$com), topic = unique(el$topic), P = unique(el$P), stringsAsFactors = F) %>% 
  left_join(el%>% group_by(com, topic, P) %>% summarise(weight.count = sum(weight)) %>% ungroup(),
            by = c("com", "topic", "P"))%>% distinct(com, topic, P, .keep_all = T) %>%
  left_join(art.com %>% select(com, P, n), by = c("com", "P")) %>% distinct(com, topic, P, .keep_all = T) %>%
  replace_na(list(weight.count = 0)) %>% 
    mutate(weight = weight.count / n) %>%
  arrange(com, topic, P) %>%
  select(com, topic, P, weight) 
el.p[is.infinite(el.p$weight) | is.nan(el.p$weight)| is.na(el.p$weight), "weight"] <- 0 

### Graph by period
com.2m.p <- bind_rows(art.com %>% group_by(com, P) %>% summarise(n = sum(n)) %>% ungroup() %>% 
                   mutate(n.rel = n / max(n)) %>% select(com, P, n.rel),
                 el %>% group_by(topic, P) %>% summarise(n = sum(weight)) %>% ungroup() %>% 
                   mutate(n.rel = n / max(n)) %>% rename(com = topic) %>% select(com, P, n.rel))


names <- com.2m.p %>% arrange(com) %>% distinct(com) %>% pull()
g.all <- graph.empty(n = length(names), directed = T) # Whole graph
V(g.all)$name <- names; rm(names)
V(g.all)$type <- substr(V(g.all)$name, 1,3) 

g.p <- list()
for (i in seq(1, length(time), 1)) { # i=1
  s <- com.2m.p %>% rename(name = com, size = n.rel) %>% arrange(name) %>% filter(P == i) %>% select(name, size)
  e <- el.p %>% rename(com1 = com, com2 = topic) %>%  mutate(weight = weight / max(weight)) 
  if(i < length(time) + 1) {e %<>% filter(P == i)}
  e %<>% select(com1, com2, weight) %>% as.matrix()
  g <- graph_from_edgelist(e[,1:2], directed = T) 
  E(g)$weight <- as.numeric(e[,3])
  g <- simplify(g, remove.loops = T, edge.attr.comb = "sum") # TODO: Why is that not symtric??????
  g <- g.all + g
  #g <- graph_attr_match(g, s, "name", c('name',"size"))
  V(g)$size <- as.numeric((strength(g) / max(strength(g))) )
  g.p[[i]] <- g
  rm(s,e,g)
}

saveRDS(el, "temp/el_2m_top.RDS")
saveRDS(el, "temp/el_2m_top_y.RDS")
saveRDS(el.p, "temp/el_2m_top__p.RDS")
saveRDS(g.p, "temp/g_top_p.RDS")


#####################################################################################################
#####################################################################################################
#####################################################################################################
### Compute diversity
#####################################################################################################
#####################################################################################################
#####################################################################################################


#####################################################################################################
##### Articles in 2-mode 
#####################################################################################################

rm(list=ls())
source("00_parameters.R")
source("00_functions.R")

art.com <- readRDS("temp/art_com_p.RDS") %>% select(com, P, n, n.cum)
cit.com <- readRDS("temp/cit_com_p.RDS") %>% select(com, P, n, n.cum, dgr.int, dgr.int.cum) # TODO: Need also the proper calculation of the periodic degrees here (see below)

names.art <- readRDS("temp/names_art.RDS")
names.cit <- readRDS("temp/names_cit.RDS")

g.art<- readRDS("temp/g_bib_p.RDS")
g.cit<- readRDS("temp/g_cit_p.RDS")
g.2m <- readRDS( "temp/g_2m_cit_p.RDS")

# Create new internal degree, since it was somewhat fucked up when we take the overall internal degree
g.all <- readRDS("temp/g_bib_final.RDS")
D.int <- data_frame()
for(i in 1:(length(time) + 1)) { # i = 1
  g <- g.all
  if(i < length(time) + 1){g <- induced.subgraph(g, V(g)[V(g)$PY >= time[i]-10 & V(g)$PY < time[i]])}
  for(j in 1:(length(names.art) + 1)) { # j = 1
    if(j < length(names.art) + 1){g.c <- induced.subgraph(g, V(g)[V(g)$com == names.art[j]])}
    x <- data_frame(com = ifelse(j < length(names.art) + 1, names.art[j], "X_all"),
                    P = i,
                    dgr.int = sum(strength(g.c)) )
    D.int %<>% bind_rows(x)
  }
}

D.int.cum <- data_frame()
for(i in 1:(length(time) + 1)) { # i = 1
  g <- g.all
  if(i < length(time) + 1){g <- induced.subgraph(g, V(g)[V(g)$PY < time[i]])}
  for(j in 1:(length(names.art) + 1)) { # j = 1
    if(j < length(names.art) + 1){g.c <- induced.subgraph(g, V(g)[V(g)$com == names.art[j]])}
    x <- data_frame(com = ifelse(j < length(names.art) + 1, names.art[j], "X_all"),
                    P = i,
                    dgr.int.cum = sum(strength(g.c)) )
    D.int.cum %<>% bind_rows(x)
  }
}

art.com %<>% left_join(D.int, by = c("com", "P"))
art.com %<>% left_join(D.int.cum, by = c("com", "P"))
rm(D.int, D.int.cum)

# Update art and cit com
x <- art.com %>% 
  group_by(P) %>%
  summarise(n = sum(n), n.cum = sum(n.cum), dgr.int = sum(dgr.int), dgr.int.cum = sum(dgr.int.cum)) %>%
  mutate(com = "X_all") %>%
  select(com, P, n, n.cum, dgr.int, dgr.int.cum)
art.com %<>% bind_rows(x) 

y <- art.com %>% filter(P == length(time)) %>%
  mutate(n = n.cum, dgr.int = dgr.int.cum, P = length(time) + 1) %>%
  select(com, P, n, n.cum, dgr.int, dgr.int.cum)

art.com %<>% bind_rows(y) %>%
  mutate(coh.int = (dgr.int) / (n * (n - 1))  ) %>%
  mutate(coh.int = if_else(is.na(coh.int) | is.nan(coh.int) | is.infinite(coh.int), 0, coh.int)) 

x <- cit.com %>% 
  group_by(P) %>%
  summarise(n = sum(n), n.cum = sum(n.cum), dgr.int = sum(dgr.int), dgr.int.cum = sum(dgr.int.cum)) %>%
  mutate(com = "X_all") %>%
  select(com, P, n, n.cum, dgr.int, dgr.int.cum)
cit.com %<>% bind_rows(x) 
y <- cit.com %>% filter(P == length(time)) %>%
  mutate(n = n.cum, dgr.int = dgr.int.cum, P = length(time) + 1) %>%
  select(com, P, n, n.cum, dgr.int, dgr.int.cum)
cit.com %<>% bind_rows(y) %>%
  mutate(coh.int = (dgr.int * 2) / (n * (n - 1))  ) %>%
  mutate(coh.int = if_else(is.na(coh.int) | is.nan(coh.int) | is.infinite(coh.int), 0, coh.int)) 
rm(x, y)

# Do the diversity calculation
D <- data_frame()
for(i in 1:(length(time) + 1)) { #  i = 1
  art <- art.com %>% filter(P == i) %>% select(com, n.cum) %>% rename(name = com)
  cit <- cit.com %>% filter(P == i) %>% select(com, n.cum) %>% rename(name = com)   
  g.a <- g.art[[i]]
  g.a <- graph_attr_match(g.a, art, "name", c('name',"n.cum"))
  g.a <- delete.vertices(g.a, V(g.a)$n.cum == 0)
  g.c <- g.cit[[i]]
  g.c <- graph_attr_match(g.c, cit, "name", c('name',"n.cum"))
  g.c <- delete.vertices(g.c, V(g.c)$n.cum == 0)
  g.2 <- g.2m[[i]]
  g.2 <- delete.vertices(g.2, !(V(g.2)$name %in% c(V(g.a)$name, V(g.c)$name)) )
  
  for(j in 1:(length(names.art) + 1)) { #  j = 6
    if(j == length(names.art) + 1){
      g.2.j <- delete.edges(g.2, E(g.2)[E(g.2)$weight < 0.025])
      g.2.j <- delete.vertices(g.2.j, V(g.2.j)[strength(g.2.j) == 0] )
      g.c.j <- delete.vertices(g.c, V(g.c)[!(V(g.c)$name %in% V(g.2.j)$name)] )
      x <- data_frame(com = "X_all",
                      P = i,
                      N = vcount(g.c),
                      n = vcount(g.c.j),
                      B = 1 - (ineq::ineq(E(g.2)$weight, type = "Gini") ),
                      D = 1 - (sum(strength(g.c.j)) / (vcount(g.c.j)*(vcount(g.c.j)-1)) ) 
                      )
      D <- bind_rows(x, D)
    } else{
      g.2.j <- delete.edges(g.2, E(g.2)[E(g.2)$weight < 0.025])
      g.2.j <- subgraph.edges(g.2.j, E(g.2.j)[inc(V(g.2.j)$name == names.art[j])]) 
      g.c.j <- delete.vertices(g.c, !(V(g.c)$name %in% V(g.2.j)$name) )
      x <- data_frame(com = names.art[j],
                      P = i,
                      N = vcount(g.c),
                      n = vcount(g.c.j),
                      B = 1 - ineq::ineq(E(g.2.j)$weight, type = "Gini"),
                      D = 1 - (sum(strength(g.c.j)) / (vcount(g.c.j)*(vcount(g.c.j)-1)) ) 
                      )
      D <- bind_rows(x, D)
    }
  }
  rm(x, g.a, g.c, g.2, g.c.j, g.2.j)
}

D %<>%
  mutate(V = n / N) %>%
  mutate(B = if_else(is.na(B) | is.nan(B) | is.infinite(B), 0, B)) %>%
  mutate(D = if_else(is.na(D) | is.nan(D) | is.infinite(D), 1, D)) %>%
  mutate(div.int = V * B * D) %>%
  left_join(art.com %>% select(com, P, coh.int), by = c("com", "P")) %>%
  select(com, P, coh.int, div.int, everything()) %>%
  distinct(com, P, .keep_all = T) %>%
  arrange(com, P)

# save
saveRDS(D, "temp/art_com_div_int.RDS")


#####################################################################################################
##### Articles in 1-mode
#####################################################################################################

rm(list=ls())
source("00_parameters.R")
source("00_functions.R")

art.com <- readRDS("temp/art_com_p.RDS") %>% select(com, P, n, n.cum) 
names.art <- readRDS("temp/names_art.RDS")

el <- readRDS("temp/el_bib_p.RDS")
g.p<- readRDS("temp/g_bib_p.RDS")

D <- data_frame()
for(i in 1:(length(time) + 1)) { #  i = 1
  if(i == length(time) + 1){
    art <- art.com %>% filter(P == i-1) %>% select(com, n.cum) %>% rename(name = com)
  } else{
    art <- art.com %>% filter(P == i) %>% select(com, n.cum) %>% rename(name = com) 
  }
  g <- g.p[[i]]
  g <- graph_attr_match(g, art, "name", c('name',"n.cum"))
  g <- delete.vertices(g, V(g)[V(g)$n.cum == 0])
  
  for(j in 1:(length(names.art) + 1)) { #  j = 3
    if(j == length(names.art) + 1){
      x <- data_frame(com = "X_all",
                      P = i,
                      N = vcount(g),
                      n = vcount(g),
                      dgr = sum(strength(g)), 
                      B = ineq::ineq(E(g)$weight, type = "Gini"),
                      D = 1
                      )
      D <- bind_rows(x, D)
      } else{
        g1 <- subgraph.edges(g, E(g)[inc(V(g)$name == names.art[j])]) # Only edges connected to J
        g2 <- induced.subgraph(g, neighbors(g, V(g)[V(g)$name == names.art[j]] ))
        g2 <- delete.vertices(g2,  V(g2)[V(g2)$name == names.art[j]] ) # Only vertices connected to j, minus j
        x <- data_frame(com = names.art[j],
                        P = i,
                        N = vcount(g),
                        n = vcount(g2),
                        dgr = ifelse(vcount(g1) == 0, 0, strength(g1, V(g1)[V(g1)$name == names.art[j]]) ), 
                        B = ineq::ineq(E(g1)$weight, type = "Gini"),
                        D = 1 - (sum(strength(g2)) / (vcount(g2)*(vcount(g2)-1)) )
        )
        D <- bind_rows(x, D)
        }
  }
  rm(x, g, g1, g2)
}

D %<>%
  mutate(V = n / N) %>%
  mutate(dgr = if_else(is.na(dgr) | is.nan(dgr) | is.infinite(dgr), 0, dgr)) %>%
  mutate(B = if_else(is.na(B) | is.nan(B) | is.infinite(B), 0, B)) %>%
  mutate(D = if_else(is.na(D) | is.nan(D) | is.infinite(D), 1, D)) %>%
  mutate(div.ext = V * B * D) %>%
  mutate(coh.ext = dgr / (N * (N-1))) %>%
  distinct(com, P, .keep_all = T) %>%
  select(com, P, coh.ext, div.ext, everything()) %>%
  arrange(com, P)

# save
saveRDS(D, "temp/art_com_div_ext.RDS")



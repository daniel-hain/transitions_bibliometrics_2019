##################################################################
##	Functions
##################################################################

read_collection <- function(file_list, bib_source, bib_format, TC_min = 0, PY_min = 0, PY_max = Inf, n_max = Inf, filter_DT = NULL) {
  require(bibliometrix); require(dplyr); require(magrittr)
  
  x <- data.frame()
  for(i in 1:length(file_list)) {# i = 2; bib_source = 'scopus'; bib_format = 'csv'; TC_min = 0; PY_min = 1990; PY_max = Inf; n_max = 1000
    cat("===== Loading bibliometric dataframe", i, "out of", length(file_list), "=====\n", sep = " ")
    
    y <- convert2df(file = file_list[i], dbsource = bib_source, format = bib_format) 
    y %<>%
      rownames_to_column('XX') %>%
      as_tibble() %>%
      filter(TC >= TC_min & PY >= PY_min & PY <= PY_max) %>%
      slice(1:n_max) %>%
      mutate(batch = i)
    # y <- y[(y[,'TC'] >= TC_min & y[,'PY']  >= PY_min & y[,'PY'] <= PY_max),]; y <- y[1:n_max,]
    
    if(!is.null(filter_DT)){
      y %<>% filter(DT %in% filter_DT)
    }
    
    count_before <- nrow(x)
    x %<>% bind_rows(y) %>%
      distinct(UT, .keep_all = TRUE)
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

##################################################################
##	BEGIN: topicmodels_json_ldavis()
##################################################################
# takes the output of a topicmodel lda and transforms it to ldaviz json
# TODO: Include option to select only certain documents, topics

topicmodels_json_ldavis <- function(fitted, doc_dtm, method = "PCA", doc_in = NULL, topic_in = NULL){
  require(topicmodels); require(dplyr); require(LDAvis)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix() # Topic-term distribution
  theta <- posterior(fitted)$topics %>% as.matrix() # Document-topic matrix
  
  # # Restrict (not working atm)
  # if(!is_null(ID_in)){theta <- theta[rownames(theta) %in%  doc_in,]; doc_fm  %<>% dfm_subset(dimnames(doc_fm)$docs %in% doc_in)}
  
  # Restrict
  if(!is_null(topic_in)){
    phi <- phi[topic_in, ]
    theta <- theta[ , topic_in]
  }
  text_tidy <- doc_dtm %>% tidy()
  vocab <- colnames(phi)
  doc_length <- tibble(document = rownames(theta)) %>% left_join(text_tidy %>% count(document, wt = count), by = 'document')
  tf <- tibble(term = vocab) %>% left_join(text_tidy %>% count(term, wt = count), by = "term") 
  
  if(method == "PCA"){mds <- jsPCA}
  if(method == "TSNE"){library(tsne); mds <- function(x){tsne(svd(x)$u)} }
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc_length %>% pull(n), term.frequency = tf %>% pull(n),
                                 reorder.topics = FALSE, mds.method = mds,plot.opts = list(xlab = "Dim.1", ylab = "Dim.2")) 
  return(json_lda)
}
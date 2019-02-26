############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")
source("functions/functions_NLP.R")

###########################################################################
# Get data out of SQL
############################################################################

# DB SQL packages
library(DBI)
library(RSQLite)
library(dbplyr)

db <- dbConnect(RSQLite::SQLite(), "output/bliographics.sqlite")
ab <- tbl(db, "transitions_2019_AB")

data <- ab %>% collect()
dbDisconnect(db)
rm(db, ab)

###########################################################################
# Some first preprocessing
###########################################################################

# Packages NLP
require(quanteda)
require(tidytext)
require(topicmodels)
require(stopwords)

## Some preprocessing
# TODO: MAke a proper stem-completion instead
data %<>% 
  mutate(TXT = paste(TI, DE, AB, sep = " ")) %>% 
  select(-TI, -AB) %>%
  mutate(TXT = TXT %>% 
           str_replace_all("&", "-and-") %>%
           str_replace_all("/(&trade;|&reg;|&copy;|&#8482;|&#174;|&#169;)/.*", "") %>%
           str_replace_all("elsevier.*", "") %>%
           str_squish() %>%
           iconv(to = "UTF-8", sub = "byte")
           )  

###########################################################################
# Generate and manipulate copus
###########################################################################

lemma_dict <- readRDS("temp/lemma_dictionary.RDS")
lemma_own <- tibble(
  stem = c("institution",   "technology",    "nation",   "region",   "sustainability", "environment",   "policy"),
  term = c("institutional", "technological", "national", "regional", "sustainable",    "environmental", "political") )
lemma_dict %<>% bind_rows(lemma_own) %>% distinct()
rm(lemma_own)

stopwords_dict <- c(stop_words %>% pull(word), stopwords("en","snowball"), stopwords("en","stopwords-iso")) %>% unique()
stopwords_own <- c("study", "paper", "result", "model", "approach", "article", "author", "method", "understand", "focus", "examine", "aim", "argue", "identify",
                   "increase", "datum", "potential", "explore", "include", "issue", "propose", "address", "apply", "require", "analyse", "relate", "finding",
                   "analyze", "discuss", "contribute", "publish", "involve", "draw", "lead", "exist", "set", "reduce", "create", "form", "explain", "play",
                   "affect", "regard", "associate", "establish", "follow", "conclude", "define", "strong", "attempt", "finally")
stopwords_dict <- c(stopwords_dict, stopwords_own) %>% unique()
rm(stopwords_own)

# Generate corpus
st_corpus <- data %>% corpus(docid_field = "EID", text_field = "TXT")
rm(data)

# Generate tokens
toks <- tokens(st_corpus, what = "word") %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_replace(pattern = lemma_dict %>% pull(term), replacement = lemma_dict %>% pull(stem)) %>%
  tokens_remove(stopwords_dict) %>%
  tokens_ngrams(n = 1:3) # %>% tokens_wordstem(language = "en") 

### Generate and adjust DTM
st_dfm <- dfm(toks) %>% 
  dfm_select(min_nchar = 3) %>%
  dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile") 
st_dfm <- st_dfm[ntoken(st_dfm) >= 2,]
st_dfm 

topwords <- topfeatures(st_dfm, 500) %>% {tibble(word = names(.), Freq = .)}

###########################################################################
# Do the LDA
###########################################################################

# Do the LDA
lda <- LDA(convert(st_dfm, to = "topicmodels"), k = 10, control = list(seed = 1337))
# saveRDS(lda, "temp/LDA_res.R"); lda <- readRDS("temp/LDA_res.R")

terms(lda, 10)
docvars(st_dfm, 'topic') <- topics(lda)
###########################################################################
# MErge with M
###########################################################################

M <- readRDS("temp/M_comleted.RDS") 
topics_terms <- tidy(lda, matrix = "beta") 
el_topic <- tidy(lda, matrix = "gamma") %>%
  rename(weight = gamma)

el_topic %<>%
  rename(EID = document) %>%
  inner_join(M %>% select(EID, PY), by = "EID") %>%
  left_join(readRDS("temp/M_nw.RDS") %>% select(EID, com), by = "EID") 

M %<>% select(EID) %>% left_join((topics(lda) %>% {tibble(EID = names(.), topic = .)}), by = "EID")

saveRDS(M, "temp/M_nlp.RDS")
saveRDS(el_topic, "temp/el_topic.RDS")
saveRDS(topics_terms, "temp/topics_terms.RDS")

###########################################################################
# LDAViz
###########################################################################

library(LDAvis)
json_lda<- topicmodels_json_ldavis(fitted = lda, doc_fm = st_dfm, method = "TSNE")
serVis(json_lda , out.dir = 'output/viz', open.browser = FALSE)

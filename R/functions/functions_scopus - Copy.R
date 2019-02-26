
##################################################################
##	BEGIN: read_scopus_csv_collection():
##################################################################
# Reads a collection of CSV obtained from scopus and parses it nicely. Wrapper around read_scopus_csv():

read_scopus_csv_collection <- function(file_list, TC_min = 0, TC_year_min = 0, PY_max = Inf, PY_min = 0, n_max = Inf, type = "REDUCED", ...) {
  x <- tibble()
  for(i in 1:length(file_list)) {
    cat("===== Loading scopus dataframe", i, "out of", length(file_list), "=====\n", sep = " ")

    y <- read_scopus_csv(file = file_list[i], TC_min = TC_min, TC_year_min = TC_year_min, PY_max = PY_max, PY_min = PY_min, n_max = n_max, type = type, ...)
    
    cat("-> Loading dataframe", i, "with", nrow(y), "rows complete\n", sep = " ")   
    x %<>% rbind(y)
  }; rm(i, y)
  
  if(type != "AB") {x %<>% arrange(desc(TC))}
  
  x %<>% distinct(EID, .keep_all = TRUE)
  
  cat("-> Done! Loading", length(file_list), "dataframes with", nrow(x), "rows complete\n", sep = " ")  
  
  return(x)
}

##################################################################
##	BEGIN: read_scopus_csv():
##################################################################
# Wrapper around different types of reading the data

read_scopus_csv <- function(file, TC_min = 0, TC_year_min = 0, PY_max = Inf, PY_min = 0, n_max = Inf, type = "REDUCED", ...){
  if(type == "AB") {
    x <- read_scopus_csv_AB(file = file, ...)
  }
  if(type == "REDUCED") {
    x <-   read_scopus_csv_REDUCED(file = file, TC_min = TC_min, TC_year_min = TC_year_min, PY_max = PY_max, PY_min = PY_min, n_max = n_max, ...)
  }
  if(type == "FULL") {
    break("FULL read is not implememented yet")
  }
  return(x)
}
  
##################################################################
##	BEGIN: read_scopus_csv_AB():
##################################################################
# For only reading abstracts
# TODO: Figure out whats the problem with DE (keywords)

read_scopus_csv_AB <- function(file){
  fread(file, data.table = FALSE, encoding = "UTF-8", header = TRUE, check.names = TRUE,
        stringsAsFactors = FALSE, strip.white = TRUE, fill = FALSE, na.strings = c("", " ",  "NA", "[No abstract available]"),
        select = c("EID", "Title", "Abstract")) %>%
    as_tibble(validate = TRUE) %>%
    rename(AB = Abstract, TI = Title) %>%
    mutate_all(str_squish) %>% 
    mutate_all(tolower) %>% 
    drop_na()
}



##################################################################
##	BEGIN: read_scopus_csv_REDUCED():
##################################################################
# Reads a CSV obtained from scopus and parses it nicely. Optimized for only reading what we need when we later anyhow extract from the API more infos

read_scopus_csv_REDUCED <- function(file, TC_min = 0, TC_year_min = 0, PY_max = Inf, PY_min = 0, n_max = Inf, ...){
  x <- fread(file, data.table = FALSE, encoding = "UTF-8", header = TRUE, check.names = TRUE,
             stringsAsFactors = FALSE, strip.white = TRUE, fill = FALSE, na.strings = c("", "NA") ) %>%
    as_tibble(validate = TRUE) %>%
    rename(AU = Authors, 
           AU_ID = Author.s..ID, 
           TI = Title, 
           SO = Source.title, 
           JI = Abbreviated.Source.Title, 
           DT = Document.Type,
           DE = Author.Keywords, 
           ID = Index.Keywords, 
           AB = Abstract, 
           #           C1 = Affiliations, 
           TC = Cited.by, 
           PY = Year, 
           #           LA = Language.of.Original.Document, 
           DI = DOI, 
           AR = Art..No., 
           BE = Editors,
           FU = Funding.Details, 
           BN = ISBN, 
           SN = ISSN, 
           PN = Page.count, 
           PU = Publisher, 
           VL = Volume, 
           IS = Issue, 
           FX = Funding.Text.1, 
           BP = Page.start, 
           EP = Page.end, 
           OS = Access.Type ) %>%
    mutate_if(is_character, str_squish) %>%   
    mutate_if(is_character, ~sub("[\\,\\;]+$", "", x = .)) %>%
    mutate_all(~na_if(., "") ) %>%
    mutate(AB = na_if(AB, "[No abstract available]")) %>%
    group_by(EID) %>% 
    mutate(AU =  map(AU, str_split, pattern = ", ") %>% flatten(),
           AU_ID =  map(AU_ID, str_split, pattern = ";") %>% flatten(),
           DE =  DE %>% tolower() %>% map(str_split, pattern = "; ") %>% flatten(),
           ID =  ID %>% tolower() %>% map(str_split, pattern = "; ") %>% flatten(),
           #           C1 =  map(C1, str_split, pattern = "; ") %>% flatten(),
           #           CR =  map(CR, str_split, pattern = "; ") %>% flatten(),
           #           NR = CR %>% list_lengths_max(),
           AU_NR = AU %>% list_lengths_max(),
           AU1 = ifelse(AU_NR == 0, NA, ifelse(AU_NR == 1, AU[[1]][1],ifelse(AU_NR == 2, paste(AU[[1]][1], AU[[1]][2], sep = " & "), paste(AU[[1]][1],"et al.", sep = " ")))),
           SR = paste0(AU1, " (" , PY, ") ", JI) ) %>%
    ungroup() %>%
    mutate(TC_year = round(TC / (2019 - PY + 1), 2) ) %>%
    select(EID, SR, AU, TI, JI, PY, TC, TC_year, DE, ID, DT, AB, AU_NR, FU, FX, OS, PU, SO, AR, IS, VL, PN, BP, EP, BE, DI, BN, SN, AU_ID, AU_NR, AU1) %>%
    filter(!is.na(EID) & !is.na(PY)) %>%
    filter(PY <= PY_max & PY >= PY_min & TC >= TC_min & TC_year >= TC_year_min) 
  
  if(n_max < nrow(x)){x %<>% slice(1:n_max)} 
  
  x %<>%
    arrange(desc(TC)) %>%
    distinct(EID, .keep_all = TRUE)  
  
  return(x)
}




##################################################################
##	BEGIN: read_scopus_csv_FULL():
##################################################################
# Reads a CSV obtained from scopus and parses it nicely. Optimized for only reading what we need when we later anyhow extract from the API more infos

read_scopus_csv_FULL <- function(file, TC_min = 0, TC_year_min = 0, PY_max = Inf, PY_min = 0, n_max = Inf, ...){
x <- fread(file, data.table = FALSE, encoding = "UTF-8", header = TRUE, check.names = TRUE,
        stringsAsFactors = FALSE, strip.white = TRUE, fill = FALSE, na.strings = c("", "NA") ) %>%
    as_tibble(validate = TRUE) %>%
    rename(AU = Authors, 
           AU_ID = Author.s..ID, 
           TI = Title, 
           SO = Source.title, 
           JI = Abbreviated.Source.Title, 
           DT = Document.Type,
           DE = Author.Keywords, 
           ID = Index.Keywords, 
           AB = Abstract, 
           C1 = Affiliations, 
           TC = Cited.by, 
           PY = Year, 
           LA = Language.of.Original.Document, 
           DI = DOI, 
           AR = Art..No., 
           BE = Editors,
           FU = Funding.Details, 
           BN = ISBN, 
           SN = ISSN, 
           PN = Page.count, 
           PU = Publisher, 
           VL = Volume, 
           IS = Issue, 
           FX = Funding.Text.1, 
           BP = Page.start, 
           EP = Page.end, 
           OS = Access.Type ) %>%
    mutate_if(is_character, str_squish) %>%   
    mutate_if(is_character, ~sub("[\\,\\;]+$", "", x = .)) %>%
    mutate_all(~na_if(., "") ) %>%
    mutate(AB = na_if(AB, "[No abstract available]")) %>%
    group_by(EID) %>% 
    mutate(AU =  map(AU, str_split, pattern = ", ") %>% flatten(),
           AU_ID =  map(AU_ID, str_split, pattern = ";") %>% flatten(),
           DE =  DE %>% tolower() %>% map(str_split, pattern = "; ") %>% flatten(),
           ID =  ID %>% tolower() %>% map(str_split, pattern = "; ") %>% flatten(),
           C1 =  map(C1, str_split, pattern = "; ") %>% flatten(),
           CR =  map(CR, str_split, pattern = "; ") %>% flatten(),
           NR = CR %>% list_lengths_max(),
           AU_NR = AU %>% list_lengths_max(),
           AU1 = ifelse(AU_NR == 0, NA, ifelse(AU_NR == 1, AU[[1]][1],ifelse(AU_NR == 2, paste(AU[[1]][1], AU[[1]][2], sep = " & "), paste(AU[[1]][1],"et al.", sep = " ")))),
           SR = paste0(AU1, " (" , PY, ") ", JI) ) %>%
    ungroup() %>%
    mutate(TC_year = round(TC / (2019 - PY + 1), 2) ) %>%
    select(EID, SR, AU, TI, JI, PY, TC, TC_year, DE, ID, DT, AB, AU_NR, FU, FX, OS, PU, SO, AR, IS, VL, PN, BP, EP, BE, DI, BN, SN, AU_ID, AU_NR, AU1) %>%
    filter(!is.na(EID) & !is.na(PY)) %>%
    filter(PY <= PY_max & PY >= PY_min & TC >= TC_min & TC_year >= TC_year_min) 

if(n_max < nrow(x)){x %<>% slice(1:n_max)} 

x %<>%
  arrange(desc(TC)) %>%
  distinct(EID, .keep_all = TRUE)  

return(x)
}


##################################################################
##	BEGIN: scopus_search_ID():
##################################################################
# Retrieves scopus articles (abstracts) via ID search. Therefore, up to 25-200 entries can be recieved for 1 request.

scopus_search_ID <- function(ID, idtype, datatype = "application/json", scopus_key, content = "complete", start = 0, retCount = 200, t_limit = 6, outfile) {
  require(httr)
  require(XML)
  require(jsonlite)
  require(tictoc)
  
  ## Some upfront checks
  if (content == "complete" & retCount > 25) {
    retCount = 25
    cat("Note: Reducing request-count to 25 for complete content retrieval. Up to 200 only available for standard content. \n")
  }
  
  ID <- unique(as.character(ID))
  resultCount <- as.numeric(length(ID)) ## get the total number of IDs
  retrievedCount <- 0 ## set the current number of records retrieved to zero
  idList <- split(ID, ceiling(seq_along(ID)/retCount)) ## split the IDs into batches 
  
  data <-  list() # Create empty list
  
  ## append the correct scopus search syntax around each number
  if (idtype == "eid") {
    idList <- lapply(mapply(paste, "EID(", idList, collapse = ") OR "), paste, ")") 
  }
  else if (idtype == "doi") {
    idList <- lapply(mapply(paste, "DOI(", idList, collapse = ") OR "), paste, ")")
  }
  else {
    stop("Invalid idtype. Valid idtypes are 'doi', or 'eid'")
  }
  cat("============================================\n")
  cat("Retrieving", resultCount, "records.\n", sep = " ")
  
  ## loop through the list of search strings and return data for each one
  for (i in 1:length(idList)) { 
    t = proc.time() # For timing the loop
    
    string <- idList[i]
    res <- httr::GET(url = "https://api.elsevier.com/content/search/scopus", 
                     query = list(apiKey = scopus_key, query = string, httpAccept = "application/json", view = content, count = retCount, start = start))
    res_parsed <- content(res, as = "parsed")
    
    ## check if there's an HTTP error
    if (httr::http_error(res) == TRUE) { ## check if there's an HTTP error
      cat("Encountered an HTTP error. Details follow.\n") ## alert the user to the error
      print(httr::http_status(res)) ## print out the error category, reason, and message
      return(data)
      break ## if there's an HTTP error, break out of the loop and return the data that has been retrieved
    }
    
    ## if the results are a list of multiple entries, get rid of one hirarchy of the list
    if (length(IDs) > 1) {
      res_parsed <- res_parsed$`search-results`$entry
    }
    data <- append(data, res_parsed)
    
    ## Update count
    retrievedCount <- retrievedCount + length(res_parsed)
    cat("Retrieved", retrievedCount, "of", resultCount, "records. \n", sep = " ")
    
    ## If necessary, put system to sleep to respect scopus quota
    tt = (proc.time() - t)[3] 
    if( tt < (1/t_limit) ) { Sys.sleep( (1/t_limit) - tt ) }
  }
  cat("Done.\n")
  cat("============================================\n")
  return(data)
}


##################################################################
##	BEGIN: scopus_document_ID():
##################################################################
# Retrieves scopus documents via ID search one-by-one in different views

scopus_document_ID <- function(ID, idtype = "eid", type = "abstract", view = "FULL", scopus_key, start = 1, t_limit = 6) {
  
  require(httr); require(XML); require(jsonlite)
  
  ID <- unique(as.character(ID))  
  path <- paste("content", type, idtype, ID, sep = "/")
  
  n <- as.numeric(length(ID)) ## get the total number of IDs
  count <- 0 ## set the current number of records retrieved to zero
  data <- vector("list", length = length(ID))
  
  cat("============================================\n")
  cat("Retrieving", n, "records of type", type, "(", view, ") \n", sep = " ")
  
  ## loop through the IDs and return data for each one
  for (i in start:length(ID)) { 
    t = proc.time() # For timing the loop
    
    res <- GET(url = "https://api.elsevier.com", path = path[i], query = list(apiKey = scopus_key,  httpAccept = "application/json", view = view) )
    res_parsed <- content(res, as = "parsed")
    
    ## check if there's an HTTP error
    if (httr::http_error(res) == TRUE) { ## check if there's an HTTP error
      cat("Encountered an HTTP error at entry:", i , "Details follow.\n") ## alert the user to the error
      print(httr::http_status(res)) ## print out the error category, reason, and message
      return(data)
      break ## if there's an HTTP error, break out of the loop and return the data that has been retrieved
    }
    
    data[[i]] <- res_parsed
    
    ## Report status
    if(i %in% seq(from = 0, to = n, by = 100)) {
      cat("Retrieved", i, "of", n, "records. \n", sep = " ")
    }
    
    ## If necessary, put system to sleep to respect scopus quota
    tt = (proc.time() - t)[3] 
    if( tt < (1/t_limit) ) { Sys.sleep( (1/t_limit) - tt ) }
  }
  cat("Done.\n")
  cat("============================================\n")
  return(data)
}


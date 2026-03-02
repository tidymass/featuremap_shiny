#' Build PubMed Query String
#'
#' Constructs a PubMed search query for a metabolite by combining its name,
#' synonyms, relevant topics, and publication date range.
#'
#' @param met_name Character string. Primary name of the metabolite.
#' @param synonyms Character vector. Alternative names/synonyms for the metabolite (optional).
#' @param years Numeric. Number of years to look back from current date (default: 5).
#'
#' @return Character string containing the formatted PubMed query.
#'
#' @examples
#' build_pubmed_query("glucose", synonyms = c("dextrose", "D-glucose"), years = 3)
build_pubmed_query <- function(met_name,
                               synonyms = NULL,
                               years = NULL) {
  
  name_part <- c(met_name, synonyms) %>% 
    unique() %>% 
    purrr::map_chr(~ sprintf('"%s"[Title/Abstract]', .x)) %>% 
    paste(collapse = " OR ")
  
  topic_part <- paste(
    c("metabolism", "biochemical", "pathway", "reaction"),
    collapse = " OR "
  )
  
  start_year <- format(Sys.Date(), "%Y") %>% as.numeric() - years
  sprintf("(%s) AND (%s) AND (%d:%d[pdat])",
          name_part,
          topic_part,
          start_year,
          format(Sys.Date(), "%Y") %>% as.numeric()
  )
}


#' Search PubMed Database
#'
#' Executes a search query against the PubMed database with retry logic
#' and rate limiting to comply with NCBI API usage policies.
#'
#' @param query Character string. The PubMed search query.
#' @param retmax Numeric. Maximum number of results to return (default: 10).
#' @param retries Numeric. Number of retry attempts on failure (default: 3).
#' @param NCBI_api_key Character string. NCBI API key for higher rate limits (optional).
#'
#' @return Character vector of PubMed IDs (PMIDs), or empty vector if search fails.
#'
#' @details
#' Without an API key, NCBI limits requests to 3 per second. With a key, this
#' increases to 10 per second. This function includes a 0.34 second delay to
#' ensure compliance with rate limits.
search_pubmed <- function(query, retmax = 10, retries = 3, NCBI_api_key = NULL) {
  
  if (!is.null(NCBI_api_key)) rentrez::set_entrez_key(NCBI_api_key)
  attempt <- 1
  while (attempt <= retries) {
    Sys.sleep(0.5)  
    res_pub <- try(
      rentrez::entrez_search(
        db = "pubmed",
        term = query,
        retmax = retmax
      ),
      silent = TRUE
    )
    if (!inherits(res_pub, "try-error")) {
      return(res_pub$ids)
    }
    attempt <- attempt + 1
  }
  
  return(character(0))
}



#' Fetch PubMed Records
#'
#' Retrieves full article titles and abstracts from PubMed for given PMIDs.
#'
#' @param pmids Character vector. PubMed IDs to fetch.
#' @param NCBI_api_key Character string. NCBI API key (optional).
#'
#' @return List containing two elements:
#'   \item{titles}{Character vector of article titles}
#'   \item{abstracts}{Character vector of article abstracts}
#'
#' @details
#' Uses XML parsing to extract structured data. Handles cases where abstracts
#' may be missing by padding with empty strings to match title length.
fetch_pubmed_records <- function(pmids, NCBI_api_key = NULL) {
  
  # Return empty lists if no PMIDs provided
  if (length(pmids) == 0) {
    return(list(titles = character(0), abstracts = character(0)))
  }
  
  if (!is.null(NCBI_api_key)) rentrez::set_entrez_key(NCBI_api_key)
  rec <- rentrez::entrez_fetch(
    db = "pubmed",
    id = pmids,
    rettype = "xml",
    parsed = FALSE
  )
  
  
  doc <- xml2::read_xml(rec)
  articles <- xml2::xml_find_all(doc, ".//PubmedArticle")
  titles <- sapply(articles, function(a) {
    t <- xml2::xml_find_first(a, ".//ArticleTitle")
    if (is.na(t)) return("")
    xml2::xml_text(t)
  })
  
  abstracts <- sapply(articles, function(a) {
    abs_nodes <- xml2::xml_find_all(a, ".//Abstract/AbstractText")
    if (length(abs_nodes) == 0) return("")
    paste(xml2::xml_text(abs_nodes), collapse = " ")
  })
  
  return(list(
    titles = titles,
    abstracts = abstracts
  ))
}


# Function for clean names of variable -----
clean_field <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(character(0))
  }
  x <- as.character(x)
  if (length(x) > 1) {
    parts <- x
  } else {
    parts <- unlist(strsplit(x, "\\{\\}"))
  }
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  parts <- unique(parts)
  parts <- parts[nchar(parts) > 1]
  return(parts)
}


#' Annotate Single Metabolite with PubMed Literature
#'
#' Searches PubMed for literature related to a specific metabolite and
#' retrieves relevant article information.
#'
#' @param met List. Metabolite information containing at least 'name' and optionally 'synonyms'.
#' @param NCBI_api_key Character string. NCBI API key (optional).
#'
#' @return List containing:
#'   \item{pmids}{Character vector of PubMed IDs}
#'   \item{titles}{Character vector of article titles}
#'   \item{abstracts}{Character vector of article abstracts}
#'
#' @details
#' Combines query building, searching, and record fetching into a single
#' workflow. Includes error handling for fetch failures.
annotate_one_metabolite_pubmed <- function(met_info, NCBI_api_key = NULL, retmax = 10, years) {
  # Build search query from metabolite name and synonyms
  query <- build_pubmed_query(
    years = years,
    met_name = met_info$name,
    synonyms = clean_field(met_info$synonyms)
  )
  
  # Search PubMed
  pmids <- search_pubmed(query, retmax = retmax, NCBI_api_key = NCBI_api_key)
  recs <- tryCatch(fetch_pubmed_records(pmids),
                   error = function(e) list(titles = NA, abstracts = NA))
  
  list(pmids = pmids, titles = recs$titles, abstracts = recs$abstracts)
  
}


#' Annotate All Metabolites in a Module with PubMed Literature
#'
#' Processes multiple metabolites sequentially, retrieving PubMed literature
#' for each one in the module.
#'
#' @param module_anno Named list. Each element is a metabolite with its metadata.
#' @param NCBI_api_key Character string. NCBI API key (optional).
#'
#' @return Named list where each element corresponds to a metabolite and contains
#'   its PubMed annotation results (PMIDs, titles, abstracts).
#'
#' @details
#' Runs sequentially (not in parallel) to comply with NCBI rate limits.
#' For parallel processing, use an API key and implement proper rate limiting.
annotate_pubmed_for_module <- function(module_anno, NCBI_api_key = NULL, retmax = 10, years) {
  
  mets <- names(module_anno)
  
  # Process each metabolite sequentially
  res_mets <- lapply(mets, function(m) {
    met_info <- module_anno[[m]]
    annotate_one_metabolite_pubmed(met_info = met_info, NCBI_api_key = NCBI_api_key, retmax = retmax, years = years)
  })
  
  names(res_mets) <- mets
  res_mets
}


#' Convert Module Data Frame to Metabolite List
#'
#' Transforms a module data frame into a structured list format suitable
#' for PubMed annotation, with one entry per unique metabolite.
#'
#' @param module_df Data frame. Must contain columns: HMDB_ID, name, synonyms,
#'   class, description, source, biospecimen_locations, tissue_locations.
#'
#' @return Named list where each element is a metabolite (named by HMDB_ID)
#'   containing standardized metadata fields.
#'
#' @details
#' Handles duplicate HMDB_IDs by taking only the first occurrence.
#' Uses clean_field() to process multi-value fields like synonyms and sources.
convert_module_to_met_list <- function(module_df, refdata = c("hmdb", "kegg")) {
  
  if(refdata == "hmdb"){
    hmdb_ids <- unique(module_df$HMDB_ID)
    met_list <- lapply(hmdb_ids, function(id) {
      df <- module_df %>% dplyr::filter(HMDB_ID == id) %>% dplyr::slice(1)
      list(
        HMDB_ID               = df$HMDB_ID,
        name                  = df$name,
        synonyms              = clean_field(df$synonyms)
      )
    })
    
    names(met_list) <- hmdb_ids
    
  }else if(refdata == "kegg"){
    kegg_ids <- unique(module_df$KEGG_ID)
    met_list <- lapply(kegg_ids, function(id) {
      df <- module_df %>% dplyr::filter(KEGG_ID == id) %>% dplyr::slice(1)
      list(
        KEGG_ID               = df$KEGG_ID,
        name                  = df$name,
        synonyms              = clean_field(df$synonyms)
      )
    })
    
    names(met_list) <- kegg_ids
  }
  
  return(met_list)
}





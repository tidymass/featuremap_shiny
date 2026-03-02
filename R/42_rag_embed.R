

## ---- Global state for rate limiting ----
.emb_call_state <- new.env(parent = emptyenv())
.emb_call_state$last_time <- NULL


#' Get Text Embeddings from API
#'
#' @param text Character vector. Text(s) to embed.
#' @param llm_api_key  Character. API token for provider.
#' @param embed_model Character. Model name (e.g. "BAAI/bge-large-en-v1.5").
#' @param provider Character. Currently supports "siliconflow".
#' @param embedding_dim Integer. Expected embedding dimension (optional check).
#'
#' @return Numeric matrix: n_texts x embedding_dim, or NULL on error.
get_embedding <- function(
    text,
    llm_api_key  ,
    embed_model = "text-embedding-3-large",
    provider = "openai", # "gemini","siliconflow"
    embedding_dim = NULL
) {
  
  ## ---- Global rate limit: avoid RPM 403 ----
  now <- Sys.time()
  if (!is.null(.emb_call_state$last_time)) {
    dt <- as.numeric(difftime(now, .emb_call_state$last_time, units = "secs"))
    min_interval <- switch(
      provider,
      "gemini" = 2,
      "openai" = 0.3,
      "siliconflow" = 0.3,
      1
    )
    if (dt < min_interval) {
      Sys.sleep(min_interval - dt)
    }
  }
  .emb_call_state$last_time <- Sys.time()
  
  ## ---- Input Validation ----
  if (is.null(text) || length(text) == 0 || all(text == "")) {
    message("Empty text input")
    return(NULL)
  }
  
  text <- text[!is.na(text) & text != ""]
  if (length(text) == 0) {
    return(NULL)
  }
  
  ## ---- Configure Provider ----
  if (provider == "siliconflow") {
    
    url <- "https://api.siliconflow.cn/v1/embeddings"
    
    body <- list(
      model = embed_model,
      input = if (length(text) == 1) text else as.list(text),
      encoding_format = "float"
    )
    
    headers <- httr::add_headers(
      Authorization = paste("Bearer", llm_api_key ),
      "Content-Type" = "application/json"
    )
  }
  else if(provider == "openai"){
    
    url <- "https://api.openai.com/v1/embeddings"
    
    body <- list(
      model = embed_model,
      input = if (length(text) == 1) text else as.list(text)
    )
    
    headers <- httr::add_headers(
      Authorization = paste("Bearer", llm_api_key),
      "Content-Type" = "application/json"
    )
    
  }
  else if(provider == "gemini"){
    
    url <- sprintf(
      "https://generativelanguage.googleapis.com/v1beta/models/%s:embedContent?key=%s",
      embed_model,
      llm_api_key
    )
    
    body <- list(
      content = list(
        parts = list(list(text = text[1]))
      )
    )
    
    headers <- httr::add_headers(
      "Content-Type" = "application/json"
    )
    
  }
  
  ## ---- Send Request ----
  res_call <- tryCatch({
    httr::POST(
      url,
      headers,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::timeout(60)
    )
  }, error = function(e) {
    message("HTTP error in get_embedding: ", e$message)
    return(NULL)
  })
  
  if (is.null(res_call)) return(NULL)
  
  ## ---- Check HTTP status ----
  if (httr::status_code(res_call) != 200) {
    message("HTTP status code: ", httr::status_code(res_call))
    message("Response: ", httr::content(res_call, "text", encoding = "UTF-8"))
    return(NULL)
  }
  
  ## ---- Parse Response ----
  cont <- tryCatch({
    httr::content(res_call, encoding = "UTF-8")
  }, error = function(e) {
    message("Failed to parse response: ", e$message)
    return(NULL)
  })
  
  if (is.null(cont)) return(NULL)
  
  if (!is.null(cont$error_msg)) {
    message("API error_msg: ", cont$error_msg)
    return(NULL)
  }
  
  if (!is.null(cont$error)) {
    message("API error: ",
            if (is.character(cont$error)) cont$error else jsonlite::toJSON(cont$error))
    return(NULL)
  }
  
  if (provider != "gemini") {
    if (is.null(cont$data) || length(cont$data) == 0) {
      message("No data field in response")
      return(NULL)
    }
  }
  
  ## ---- Extract Embeddings ----
  embs <- tryCatch({
    
    if (provider == "gemini") {
      list(unlist(cont$embedding$values))
    } else {
      lapply(cont$data, function(x) unlist(x$embedding))
    }
  }, error = function(e) {
    message("Failed to extract embeddings: ", e$message)
    return(NULL)
  })
  
  if (is.null(embs)) return(NULL)
  
  emb_matrix <- tryCatch({
    do.call(rbind, embs)
  }, error = function(e) {
    message("Failed to combine embeddings: ", e$message)
    return(NULL)
  })
  
  ## ---- Validate dimensions ----
  if (!is.null(emb_matrix)) {
    if (nrow(emb_matrix) != length(text)) {
      warning(sprintf("Dimension mismatch: %d texts but %d embeddings",
                      length(text), nrow(emb_matrix)))
    }
    
    if (!is.null(embedding_dim) && ncol(emb_matrix) != embedding_dim) {
      warning(sprintf("Unexpected embedding dimension: expected %d but got %d",
                      embedding_dim, ncol(emb_matrix)))
    }
  }
  
  return(emb_matrix)
}


#' Detect Embedding Dimension from API
#'
#' @return Integer embedding dimension, or NULL if detection fails.
detect_embedding_dim <- function(llm_api_key ,
                                 embed_model = "BAAI/bge-large-en-v1.5",
                                 provider = "siliconflow") {
  
  message("Detecting embedding dimension for model: ", embed_model)
  
  test_emb <- get_embedding(
    text = "test",
    llm_api_key  = llm_api_key ,
    embed_model = embed_model,
    provider = provider,
    embedding_dim = NULL
  )
  
  if (is.null(test_emb)) {
    warning("Failed to detect embedding dimension - API call failed")
    return(NULL)
  }
  
  dim_detected <- ncol(test_emb)
  message("Detected embedding dimension: ", dim_detected)
  
  return(dim_detected)
}


#' Get Embedding Dimension for Model
#'
#' @return Integer dimension, or NULL.
get_embedding_dim <- function(embed_model,
                              llm_api_key  = NULL,
                              provider = "siliconflow") {
  
  model_dims <- list(
    "BAAI/bge-large-en-v1.5" = 1024,
    "BAAI/bge-base-en-v1.5"  = 768,
    "BAAI/bge-small-en-v1.5" = 384,
    "text-embedding-ada-002" = 1536,
    "text-embedding-3-small" = 1536,
    "text-embedding-3-large" = 3072,
    "embedding-001" = 768
  )
  
  if (embed_model %in% names(model_dims)) {
    return(model_dims[[embed_model]])
  }
  
  if (!embed_model %in% names(model_dims) && !is.null(llm_api_key )) {
    dim_detected <- detect_embedding_dim(
      llm_api_key = llm_api_key ,
      embed_model = embed_model,
      provider    = provider
    )
    if (!is.null(dim_detected)) return(dim_detected)
  }
  
  warning("Could not determine embedding dimension for model: ", embed_model)
  warning("Provide llm_api_key  to auto-detect, or add model to known_models list")
  return(NULL)
}


#' Build Metabolite Query Texts
build_metabolite_queries <- function(met_name) {
  list(
    biochem  = sprintf("Biochemical function of %s", met_name),
    physio   = sprintf("Endogenous metabolism and physiological role of %s", met_name),
    reaction = sprintf("Metabolic reactions involving %s", met_name)
  )
}


#' Calculate Cosine Similarity
cosine_sim <- function(a, b) {
  if (length(a) != length(b)) {
    warning("Vector dimensions do not match")
    return(0)
  }
  
  denom <- sqrt(sum(a^2)) * sqrt(sum(b^2))
  if (denom == 0) return(0)
  sum(a * b) / denom
}


#' Precompute PubMed Abstract Embeddings for Module
#'
#' @param module_pub Named list, each element: list(pmids, titles, abstracts)
#' @return Named list: each metabolite -> list(pmids, titles, abstracts, emb)
precompute_pubmed_embeddings_for_module <- function(
    module_pub,
    llm_api_key ,
    embed_model,
    provider,
    max_chars_per_chunk = 1500,
    embedding_dim = NULL
) {
  
  if (is.null(embedding_dim)) {
    stop("Cannot determine embedding dimension for model: ", embed_model,
         ". Please check your API connection.")
  }
  
  mets <- names(module_pub)
  module_cache <- list()
  
  for (m in mets) {
    pubmed_obj <- module_pub[[m]]
    titles     <- pubmed_obj$titles
    abstracts  <- pubmed_obj$abstracts
    pmids      <- pubmed_obj$pmids
    
    valid <- which(!is.na(abstracts) & abstracts != "")
    if (length(valid) == 0) {
      next
    }
    
    abstracts <- abstracts[valid]
    titles    <- titles[valid]
    pmids     <- pmids[valid]
    
    n_docs <- length(abstracts)
    doc_emb_list <- vector("list", n_docs)
    
    n_chars   <- nchar(abstracts, type = "chars")
    idx_short <- which(n_chars <= max_chars_per_chunk)
    idx_long  <- which(n_chars >  max_chars_per_chunk)
    
    if (length(idx_short) > 0) {
      
      if (provider == "gemini") {
        # Gemini: embed one-by-one
        for (i in idx_short) {
          emb_i <- get_embedding(
            text          = abstracts[i],
            llm_api_key   = llm_api_key,
            embed_model   = embed_model,
            provider      = provider,
            embedding_dim = embedding_dim
          )
          
          if (is.null(emb_i)) {
            doc_emb_list[[i]] <- rep(0, embedding_dim)
          } else {
            doc_emb_list[[i]] <- as.numeric(emb_i[1, ])
          }
        }
        
      } else {
        # OpenAI / SiliconFlow: vectorized embedding
        emb_short <- get_embedding(
          text          = abstracts[idx_short],
          llm_api_key   = llm_api_key,
          embed_model   = embed_model,
          provider      = provider,
          embedding_dim = embedding_dim
        )
        
        if (is.null(emb_short)) {
          for (i in idx_short) {
            doc_emb_list[[i]] <- rep(0, embedding_dim)
          }
        } else {
          for (k in seq_along(idx_short)) {
            i <- idx_short[k]
            doc_emb_list[[i]] <- as.numeric(emb_short[k, ])
          }
        }
      }
    }
    
    
    if (length(idx_long) > 0) {
      for (i in idx_long) {
        txt <- abstracts[i]
        n   <- nchar(txt, type = "chars")
        starts <- seq(1, n, by = max_chars_per_chunk)
        ends   <- pmin(starts + max_chars_per_chunk - 1, n)
        chunks <- substring(txt, starts, ends)
        
        if (provider == "gemini") {
          # Gemini: one chunk per call
          emb_list <- lapply(chunks, function(ch) {
            get_embedding(
              text          = ch,
              llm_api_key   = llm_api_key,
              embed_model   = embed_model,
              provider      = provider,
              embedding_dim = embedding_dim
            )
          })
          
          emb_list <- emb_list[!sapply(emb_list, is.null)]
          
          if (length(emb_list) == 0) {
            doc_emb_list[[i]] <- rep(0, embedding_dim)
          } else {
            emb_mat <- do.call(rbind, emb_list)
            doc_emb_list[[i]] <- colMeans(emb_mat)
          }
          
        } else {
          # OpenAI / SiliconFlow: vectorized input OK
          chunk_emb <- get_embedding(
            text          = chunks,
            llm_api_key   = llm_api_key,
            embed_model   = embed_model,
            provider      = provider,
            embedding_dim = embedding_dim
          )
          
          if (is.null(chunk_emb)) {
            doc_emb_list[[i]] <- rep(0, embedding_dim)
          } else {
            doc_emb_list[[i]] <- colMeans(chunk_emb)
          }
        }
      }
    }
    
    
    # 3) list -> matrix
    emb_mat <- do.call(rbind, doc_emb_list)
    
    if (!is.null(emb_mat) && ncol(emb_mat) != embedding_dim) {
      warning("Unexpected embedding dimension for ", m,
              ": expected ", embedding_dim, " but got ", ncol(emb_mat))
    }
    
    module_cache[[m]] <- list(
      pmids     = pmids,
      titles    = titles,
      abstracts = abstracts,
      emb       = emb_mat
    )
  }
  
  return(module_cache)
}


#' Embed and Rank PubMed Articles for Single Metabolite
#'
#' @param abs_embeds Optional precomputed embeddings (rows = abstracts)
#' @return List with pmids/titles/abstracts/scores for top_score docs, or NULL.
embed_pubmed_for_metabolite <- function(
    met_name,
    pubmed_obj,
    met_real_name,
    similarity,
    top_score = 5,
    llm_api_key ,
    embed_model = "BAAI/bge-large-en-v1.5",
    provider    = "siliconflow",
    abs_embeds  = NULL,
    embedding_dim = NULL
) {
  
  titles    <- pubmed_obj$titles
  abstracts <- pubmed_obj$abstracts
  pmids     <- pubmed_obj$pmids
  
  valid <- which(!is.na(abstracts) & abstracts != "")
  if (length(valid) == 0) return(NULL)
  
  titles    <- titles[valid]
  abstracts <- abstracts[valid]
  pmids     <- pmids[valid]
  
  # 1) query embedding
  queries <- unname(unlist(build_metabolite_queries(met_real_name)))
  
  if (provider == "gemini") {
    q_emb_list <- lapply(queries, function(q) {
      get_embedding(
        text          = q,
        llm_api_key   = llm_api_key,
        embed_model   = embed_model,
        provider      = provider,
        embedding_dim = embedding_dim
      )
    })
    
    q_emb_list <- q_emb_list[!sapply(q_emb_list, is.null)]
    
    if (length(q_emb_list) == 0) {
      message("Failed to get query embeddings for metabolite: ", met_name)
      return(NULL)
    }
    
    query_embeds <- do.call(rbind, q_emb_list)
    
  } else {
    query_embeds <- get_embedding(
      text          = queries,
      llm_api_key   = llm_api_key,
      embed_model   = embed_model,
      provider      = provider,
      embedding_dim = embedding_dim
    )
  }
  
  
  if (is.null(query_embeds)) {
    message("Failed to get query embeddings for metabolite: ", met_name)
    return(NULL)
  }
  
  # 2) abstract embedding
  if (!is.null(abs_embeds)) {
    abs_embeds_use <- abs_embeds
    
    if (nrow(abs_embeds_use) != length(abstracts)) {
      warning("Number of embeddings (", nrow(abs_embeds_use),
              ") does not match number of abstracts (", length(abstracts),
              ") for ", met_name)
      n_min <- min(nrow(abs_embeds_use), length(abstracts))
      abs_embeds_use <- abs_embeds_use[seq_len(n_min), , drop = FALSE]
      abstracts      <- abstracts[seq_len(n_min)]
      titles         <- titles[seq_len(n_min)]
      pmids          <- pmids[seq_len(n_min)]
    }
    
    if (!is.null(embedding_dim) && ncol(abs_embeds_use) != ncol(query_embeds)) {
      stop("Embedding dimension mismatch between abstracts and queries")
    }
    
  } else {
    abs_embeds_use <- get_embedding(
      text          = abstracts,
      llm_api_key        = llm_api_key ,
      embed_model   = embed_model,
      provider      = provider,
      embedding_dim = embedding_dim
    )
    
    if (is.null(abs_embeds_use)) {
      message("Failed to get abstract embeddings for metabolite: ", met_name)
      return(NULL)
    }
  }
  
  # 3) similarity scores
  sim_scores <- sapply(1:nrow(abs_embeds_use), function(i) {
    abs_e <- abs_embeds_use[i, ]
    mean(apply(query_embeds, 1, function(qe) cosine_sim(abs_e, qe)))
  })
  
  sim_scores[is.na(sim_scores) | is.infinite(sim_scores)] <- 0
  
  keep_idx <- which(sim_scores >= similarity)
  
  if (length(keep_idx) == 0) {
    message("No PubMed articles pass similarity threshold for metabolite: ", met_name)
    return(NULL)
  }
  
  # 4) top_score
  ordered_keep <- keep_idx[order(sim_scores[keep_idx], decreasing = TRUE)]
  top_id <- ordered_keep[seq_len(min(top_score, length(ordered_keep)))]
  
  return(list(
    met_name  = met_real_name,
    pmids     = pmids[top_id],
    titles    = titles[top_id],
    abstracts = abstracts[top_id],
    scores    = sim_scores[top_id]
  ))
}


#' Embed and Rank PubMed Articles for All Metabolites in Module
#'
#' @param module_pub Named list: per metabolite PubMed data (pmids, titles, abstracts)
#' @param emb_cache Optional named list from precompute_pubmed_embeddings_for_module()
#' @return Named list: per metabolite ranking结果
embed_module_pubmed <- function(module_pub,
                                llm_api_key,
                                refdata = c("hmdb","kegg"),
                                module_df   = NULL,
                                embed_model = "BAAI/bge-large-en-v1.5",
                                provider    = "siliconflow",
                                emb_cache   = NULL,
                                top_score   = 5,
                                min_similarity = NULL,
                                embedding_dim = NULL) {
  
  mets <- names(module_pub)
  rak  <- list()
  
  for (m in mets) {
    
    met_real_name <- m
    if(refdata == "hmdb"){
      if (!is.null(module_df) && "HMDB_ID" %in% colnames(module_df)) {
        hit <- module_df[module_df$HMDB_ID == m, "name"]
        if (length(hit) > 0) met_real_name <- hit[1]
      }
    }else if(refdata == "kegg"){
      if (!is.null(module_df) && "KEGG_ID" %in% colnames(module_df)) {
        hit <- module_df[module_df$KEGG_ID == m, "name"]
        if (length(hit) > 0) met_real_name <- hit[1]
      }
    }
    
    
    if (!is.null(emb_cache) && !is.null(emb_cache[[m]])) {
      cache_m <- emb_cache[[m]]
      pubmed_obj_use <- list(
        pmids     = cache_m$pmids,
        titles    = cache_m$titles,
        abstracts = cache_m$abstracts
      )
      abs_embeds_use <- cache_m$emb
    } else {
      pubmed_obj_use <- module_pub[[m]]
      abs_embeds_use <- NULL
    }
    
    rak[[m]] <- tryCatch({
      embed_pubmed_for_metabolite(
        met_name       = m,
        met_real_name  = met_real_name,
        pubmed_obj     = pubmed_obj_use,
        llm_api_key    = llm_api_key ,
        embed_model    = embed_model,
        provider       = provider,
        abs_embeds     = abs_embeds_use,
        top_score      = top_score,
        embedding_dim  = embedding_dim,
        similarity = min_similarity
      )
    }, error = function(e) {
      message("Error processing ", m, ": ", e$message)
      return(NULL)
    })
  }
  
  rak <- rak[!sapply(rak, is.null)]
  return(rak)
}

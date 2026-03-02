


# silicon: sk-simkoghzjiotrfnphgbbqljhltgbkrckhrvojoccdodudenf
# gpt: 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
# gemini: AIzaSyDkqVJLeAcI5ezqmNLJdsWSBDQnSjiaQ_8
annotate_module = function(res = res,
                           p = "fdr",
                           pcutoff = 0.05,
                           ref_data = NULL,
                           species = "human",
                           biospecimen = NULL,
                           phenotype = NULL,
                           pubmed = TRUE,
                           top_compound = 0.1,
                           years = 5,
                           top_paper = 3,
                           min_similarity = 0.6,
                           provider    = "openai",
                           embed_model = "text-embedding-3-small",
                           llm_model = "gpt-4.1-mini",
                           NCBI_api_key = "5da89a5ddb814e3c26c95f68bfdb4ad39309", # https://account.ncbi.nlm.nih.gov/settings/
                           llm_api_key = ""){

  
  # Annotate metabolites via database ------------------------------------------
  message("Step 1: Annotating the module via well-established database ...\n")
  
  if(res$refdata == "hmdb"){
    sig_module <- res$module_table %>% 
      mutate(Robustness = Node_freq) %>% 
      filter(Module %in% (res$module_score %>% filter(.data[[p]]<pcutoff) %>% pull(Module))) %>% 
      arrange(Module, desc(Importance), desc(Robustness), Type) %>% 
      left_join(ref_data, by = c("HMDB_ID" = "id"))
  }else if(res$refdata == "kegg"){
    sig_module <- res$module_table %>% 
      mutate(Robustness = Node_freq) %>% 
      filter(Module %in% (res$module_score %>% filter(.data[[p]]<pcutoff) %>% pull(Module))) %>% 
      arrange(Module, desc(Importance), desc(Robustness), Type) %>% 
      left_join(ref_data, by = c("KEGG_ID" = "id"))
  }
  
  if (is.null(sig_module) || nrow(sig_module) == 0) {
    message("No significant modules passed the cutoff (", p, " < ", pcutoff, "). Returning empty module_function.\n")
    res$module_function <- tibble::tibble()
    return(res)
  }
  
  
  module_list <- sig_module %>%
    split(.$Module) %>%                   
    set_names(paste0("module", names(.)))
  
  module_list_top <- lapply(module_list, function(module) {
    
    n_mod <- nrow(module)
    topN <- min(ceiling(n_mod * top_compound), 20)
    
    module %>%
      mutate(weighted_score = 0.7 * Importance + 0.3 * Robustness) %>%
      arrange(desc(weighted_score)) %>%
      slice_head(n = topN)
  })
  
  
  
  if(res$refdata == "hmdb"){
    module_class <- imap(
      module_list,
      ~ {
        module_name <- .y
        mod_non_top <- .x
        
        if (nrow(mod_non_top) == 0) {
          return(tibble())
        }
        
        mod_non_top %>%
          dplyr::group_by(class) %>%
          dplyr::summarise(
            n_metabolites = n(),
            metabolites = paste(unique(Compound_name), collapse = "{}"),
            HMDB_IDs = paste(unique(HMDB_ID), collapse = "{}"),
            mean_importance = mean(Importance, na.rm = TRUE),
            mean_robustness = mean(Robustness, na.rm = TRUE),
            .groups = "drop")})
    
  }else if(res$refdata == "kegg"){
    module_class <- imap(
      module_list,
      ~ {
        module_name <- .y
        mod_non_top <- .x
        
        if (nrow(mod_non_top) == 0) {
          return(tibble())
        }
        
        mod_non_top %>%
          group_by(class) %>%
          dplyr::summarise(
            n_metabolites = n(),
            metabolites = paste(unique(Compound_name), collapse = "{}"),
            KEGG_IDs = paste(unique(KEGG_ID), collapse = "{}"),
            mean_importance = mean(Importance, na.rm = TRUE),
            mean_robustness = mean(Robustness, na.rm = TRUE),
            .groups = "drop")})
  }
  
  
  module_statistics <- lapply(module_list, function(module) {
    list(
      Importance = mean(module$Importance, na.rm = TRUE),
      Robustness = mean(module$Robustness, na.rm = TRUE),
      Annotated_fraction = sum(module$Type == "annotated") / nrow(module)
    )
  })
  
  
  message("Step 1 has finished!\n")
  
  # Annotate metabolites via pubmed --------------------------------------------
  
  if(pubmed){
    
    message("Step 2: Annotating the module via pubmed literatures...\n")
    
    rentrez::set_entrez_key(NCBI_api_key)
    
    message("-Searching related literatures via PubMed...")

    
    module_pubmed <- lapply(names(module_list_top), function(mod) {
      message("--Searching literatures for: ", mod)
      module_df  <- module_list_top[[mod]]
      module_anno <- convert_module_to_met_list(module_df, refdata = res$refdata)
      annotate_pubmed_for_module(module_anno = module_anno, NCBI_api_key = NCBI_api_key, retmax = 10, years = years)
    })
    
    names(module_pubmed) <- names(module_list_top)
    
    # Embed pubmed-based annotation results and choose those with similarity > 0.5 -------
    
    message("\n-Embedding the literatures via ", embed_model, "...")
    
    embedding_dim <- get_embedding_dim(
      embed_model = embed_model,
      llm_api_key = llm_api_key,
      provider    = provider
    )
    
    
    module_pubmed_embed <- lapply(names(module_pubmed), function(mod) {
      
      message("--Embedding for ", mod)
      
      emb_cache_mod <- precompute_pubmed_embeddings_for_module(
        module_pub  = module_pubmed[[mod]],
        llm_api_key = llm_api_key,
        embed_model = embed_model,
        provider    = provider,
        embedding_dim = embedding_dim)
      
      embed_module_pubmed(
        module_pub  = module_pubmed[[mod]], 
        module_df   = module_list_top[[mod]],
        llm_api_key = llm_api_key,
        provider    = provider,
        embed_model = embed_model,
        emb_cache   = emb_cache_mod,
        embedding_dim = embedding_dim,
        top_score   = 5,
        refdata = res$refdata,
        min_similarity = min_similarity)
      
    })
    
    names(module_pubmed_embed) <- names(module_pubmed)
    
    
    # Re-rank and Summarize the pubmed-based annotation ------------------------
    message("\n-Reranking the related literatures via ", llm_model, "...")
    
    module_pubmed_embed_refined <- refine_module_pubmed_with_llm(
      module_pubmed_embed = module_pubmed_embed,
      llm_api_key         = llm_api_key,
      llm_model           = llm_model,
      provider            = provider,
      top_paper           = top_paper,
      max_chars_abstract  = 1500,
      sleep_seconds       = 1)
    
    names(module_pubmed_embed_refined) <- names(module_pubmed_embed)
    
    message("\n-Summarizing the top related literatures for final pubmed-based annotation...")
    
    module_pubmed_embed_refined <- add_metabolite_summaries_to_modules(
      module_pubmed_embed_refined,
      provider = provider,
      llm_api_key = llm_api_key,
      llm_model   = llm_model)
    
    message("\nStep 2 has finished!\n")
    
    message("Step 3: Generating prompt for LLM-based functional annotation...\n")
    
  }else{
    
    module_pubmed_embed_refined = NULL
    
    message("Step 2: Generating prompt for LLM-based functional annotation...\n")
  }
  
  # Generate input prompt to LLM module ----------------------------------------
  
  all_modules_ready <- lapply(
    seq_along(module_list_top),
    function(i) {
      prepare_module(
        k = i,
        module_list_top = module_list_top,
        module_list = module_list,
        module_class = module_class,
        module_pubmed_embed_refined = module_pubmed_embed_refined,
        module_statistics = module_statistics,
        pubmed = pubmed
      )
    }
  )
  
  names(all_modules_ready) <- names(module_list_top)
  
  study_context <- list(
    species = species,
    biospecimen = biospecimen,
    phenotype = phenotype
  )
  
  all_module_prompts <- lapply(
    all_modules_ready,
    build_module_prompt,
    study_context = study_context)
  
  names(all_module_prompts) <- names(all_modules_ready)
  
  if(pubmed){
    message("Step 3 has finished!\n")
  }else{
    message("Step 2 has finished!\n")
  }
  
  
  # Use LLM module to annotate functional module -------------------------------
  if(pubmed){
    message("Step 4: Annotating the module via large language model...\n")
  }else{
    message("Step 3: Annotating the module via large language model...\n")
  }
  
  if (length(all_modules_ready) == 0) {
    message("No modules to annotate. Returning empty module_function.\n")
    res$module_function <- tibble::tibble()
    return(res)
  }
  
  all_results <- annotate_all_modules(
    all_modules_ready = all_modules_ready,
    study_context     = study_context,
    provider          = provider,
    llm_api_key       = llm_api_key,
    model             = llm_model, 
    max_tokens        = 600,
    temperature       = 0,
    sleep_sec         = 0)
  
  
  res$module_function = all_results
  
  if(pubmed){
    message("\nStep 4 has finished!\n")
  }else{
    message("\nStep 3 has finished!\n")
  }
  
  return(res)
  
}


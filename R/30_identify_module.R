

#' Identify modules via community detection
#'
#' @param res Result object containing the PCSF subnetwork.
#' @param method Community detection method.
#' @param min_size Minimum module size.
#' @param max_size Maximum module size.
#' @param n_perm Number of permutations for module scoring.
#' @param seed Random seed.
#' @param workers Number of parallel workers.
#' @param node_freq Minimum retained node frequency.
#' @param louvain_resolution Resolution used for Louvain clustering.
#' @param walktrap_steps Number of steps for walktrap clustering.
#'
#' @return Updated result object with detected modules and module scores.
#' @export
#'
#' @examples
#' \dontrun{
#' identify_module(res)
#' }

identify_module = function(res = NULL,
                           method = c("louvain","leading_eigen","fast_greedy","walktrap"),
                           min_size = 3,
                           max_size = 100000,
                           n_perm = 100,
                           seed = 123,
                           workers = 4,
                           node_freq = 0.05,
                           louvain_resolution = 1,
                           walktrap_steps = 4){
  
  if (is.null(res)){
    stop("[Error] res cannot be NULL.")
  }
  
  start_time <- Sys.time()
  
  set.seed(seed)
  
  # prepare node of sub-network for community detection ------------------------
  ## annotated metabolites INCHI IDs---
  res$edge_frequency %>% 
    filter(!str_detect(to, "InChI")) %>% 
    pull(from) -> annotated_ID1
  
  res$edge_frequency %>% 
    filter(!str_detect(from, "InChI")) %>% 
    pull(to) -> annotated_ID2
  
  c(annotated_ID1, annotated_ID2) -> annotated_ID
  
  ## All metabolites INCHI ID from MRN---
  mrn_node <- as.data.frame(igraph::as_data_frame(res$mrn, what = "vertices"))
  
  ## Steiner_nodes without low frequency---
  res$node_frequency %>% 
    filter(type == "metabolite") %>% 
    filter(freq > node_freq) %>% 
    pull(name) -> res_node
  
  steiner_nodes <- V(res$pcsf_subnet)[V(res$pcsf_subnet)$type == "Steiner"]$name
  steiner_nodes <- steiner_nodes[steiner_nodes %in% res_node]
  
  ## Induced subgraph based on steiner nodes---
  steiner_subnet <- induced_subgraph(res$mrn, vids = steiner_nodes)
  steiner_subnet <- igraph::set_vertex_attr(
    steiner_subnet,
    "type",
    value = ifelse(igraph::V(steiner_subnet)$name %in% annotated_ID, "annotated", "hidden")
  )
  
  # prepare edge weights for community detection -------------------------------
  res$edge_frequency %>% 
    filter(str_detect(from, "InChI") & str_detect(to, "InChI")) %>% 
    dplyr::rename(weight = freq) -> edge_weight
  
  g_edges <- igraph::as_data_frame(steiner_subnet, what = "edges")
  g_edges$u <- pmin(g_edges$from, g_edges$to)
  g_edges$v <- pmax(g_edges$from, g_edges$to)
  
  edge_weight$u <- pmin(edge_weight$from, edge_weight$to)
  edge_weight$v <- pmax(edge_weight$from, edge_weight$to)
  
  g_edges$key      <- paste(g_edges$u,      g_edges$v,      sep = "_")
  edge_weight$key  <- paste(edge_weight$u,  edge_weight$v,  sep = "_")
  
  idx <- match(g_edges$key, edge_weight$key)
  w <- edge_weight$weight[idx]
  mean_w <- mean(edge_weight$weight, na.rm = TRUE)
  w[is.na(w)] <- mean_w
  
  # E(steiner_subnet)$weight <- w
  # ---- HUB PENALTY USING FULL MRN DEGREE ----
  mrn_deg <- igraph::degree(res$mrn)
  edge_ends <- igraph::ends(steiner_subnet, igraph::E(steiner_subnet))
  w_adj <- w / sqrt(mrn_deg[edge_ends[,1]] * mrn_deg[edge_ends[,2]])
  w_adj[!is.finite(w_adj)] <- min(w_adj[is.finite(w_adj)], na.rm = TRUE)
  steiner_subnet <- igraph::set_edge_attr(steiner_subnet, "weight", value = w_adj)
  
  
  # module detection -----------------------------------------------------------
  if (method == "louvain") {
    comm <- igraph::cluster_louvain(steiner_subnet, weights = E(steiner_subnet)$weight, resolution = louvain_resolution)
  } else if (method == "fast_greedy") {
    comm <- igraph::cluster_fast_greedy(igraph::as_undirected(steiner_subnet), weights = E(steiner_subnet)$weight)
  } else if (method == "walktrap") {
    comm <- igraph::cluster_walktrap(steiner_subnet, weights = E(steiner_subnet)$weight, steps = walktrap_steps)
  } else if (method == "leading_eigen") {
    comm <- igraph::cluster_leading_eigen(steiner_subnet, weights = E(steiner_subnet)$weight)
  }
  
  member <- igraph::membership(comm)
  
  # construct the module table ---
  if(res$refdata == "hmdb"){
    
    res$module_table <- data.frame(
      name = names(member),
      module = as.integer(member)) %>%
      dplyr::arrange(module) %>%
      dplyr::group_by(module) %>%
      dplyr::filter(dplyr::n() >= min_size & dplyr::n() <= min(max_size, vcount(steiner_subnet))) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(mrn_node[,c("HMDB_ID", "INCHI_ID", "Compound_name")], by = c("name" = "INCHI_ID")) %>% 
      dplyr::left_join((res$node_frequency %>% dplyr::select(-type)), by = c("name")) %>% 
      dplyr::rename(INCHI_ID = name) %>% 
      dplyr::left_join(data.frame(
        INCHI_ID = V(steiner_subnet)$name,
        Type = V(steiner_subnet)$type), by = "INCHI_ID")  
    
    res$anno_feature_table2 %>% 
      filter(INCHI_ID %in% res$module_table$INCHI_ID) %>% 
      dplyr::distinct(variable_id, INCHI_ID, .keep_all = TRUE) %>% 
      dplyr::left_join(res$feature_table[,c("variable_id","importance")], by = "variable_id") %>% 
      dplyr::group_by(INCHI_ID) %>% 
      dplyr::summarise(
        Importance = {
          w <- weighted_anno_score
          x <- importance
          if (all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
            mean(x, na.rm = TRUE)
          } else {
            stats::weighted.mean(x, w, na.rm = TRUE)
          }
        }
      ) %>% 
      dplyr::ungroup() -> meta_importance
    
    res$module_table %>% 
      left_join(meta_importance, by = "INCHI_ID") %>% 
      purrr::set_names(c("INCHI_ID", "Module", "HMDB_ID", "Compound_name", "Node_freq", "Type", "Importance")) -> res$module_table
    
  }else if(res$refdata == "kegg"){
    
    res$module_table <- data.frame(
      name = names(member),
      module = as.integer(member)) %>%
      dplyr::arrange(module) %>%
      dplyr::group_by(module) %>%
      dplyr::filter(dplyr::n() >= min_size & dplyr::n() <= min(max_size, vcount(steiner_subnet))) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(mrn_node[,c("KEGG_ID", "INCHI_ID", "Compound_name")], by = c("name" = "INCHI_ID")) %>% 
      dplyr::left_join((res$node_frequency %>% dplyr::select(-type)), by = c("name")) %>% 
      dplyr::rename(INCHI_ID = name) %>% 
      dplyr::left_join(data.frame(
        INCHI_ID = V(steiner_subnet)$name,
        Type = V(steiner_subnet)$type), by = "INCHI_ID")  
    
    res$anno_feature_table2 %>% 
      filter(INCHI_ID %in% res$module_table$INCHI_ID) %>% 
      dplyr::distinct(variable_id, INCHI_ID, .keep_all = TRUE) %>% 
      dplyr::left_join(res$feature_table[,c("variable_id","importance")], by = "variable_id") %>% 
      dplyr::group_by(INCHI_ID) %>% 
      dplyr::summarise(
        Importance = {
          w <- weighted_anno_score
          x <- importance
          if (all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
            mean(x, na.rm = TRUE)
          } else {
            stats::weighted.mean(x, w, na.rm = TRUE)
          }
        }
      ) %>% 
      dplyr::ungroup() -> meta_importance
    
    res$module_table %>% 
      left_join(meta_importance, by = "INCHI_ID") %>% 
      purrr::set_names(c("INCHI_ID", "Module", "KEGG_ID", "Compound_name", "Node_freq", "Type", "Importance")) -> res$module_table
    
  }
  
  
  message(sprintf("Detected %d modules using %s method.", length(unique(res$module_table$Module)), method))
  
  
  ## Calculate module activity scores ----
  message("\nCalculating module scores for each module ...")
  
  read_importance_tbl<- res$module_table %>%
    dplyr::filter(Type == "annotated", is.finite(Importance)) %>%
    dplyr::select(INCHI_ID, Importance) %>%
    dplyr::distinct(INCHI_ID, .keep_all = TRUE)
  
  res$module_score <- calculate_module_activity(
    res = res,
    annotated_ID = annotated_ID,
    mrn = res$mrn,
    importance_tbl = read_importance_tbl)
  
  ## Calculate p values for each module via permutation test ---
  
  message("\nPerforming permutation test for module significance ...\n")
  
  perm_p <- permutation_test_gamma(
    subnet = steiner_subnet,
    mrn = res$mrn,
    module_table = res$module_table,
    module_score = res$module_score,
    method = method,
    min_size = min_size,
    n_perm = n_perm,
    workers = workers,
    seed = seed
  )
  
  res$module_score <- res$module_score %>%
    dplyr::left_join(perm_p[,c("Module","p","fdr")], by = "Module")
  
  message("\nStep 3: Module identification has finished!\n")
  
  if(nrow(res$module_score) == 0){
    message("\nNo module was detected successfully!\n")
  }
  
  end_time <- Sys.time()
  cat("Execution time:", round(as.numeric(end_time - start_time, units = "mins"), 2), "minutes\n")
  
  
  return(res)
  
}



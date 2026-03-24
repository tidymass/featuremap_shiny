#' Extract a sub-network using the PCSF algorithm
#'
#' Construct a prize-collecting Steiner forest (PCSF)-based metabolic
#' sub-network from annotated metabolomics features and a reference
#' metabolic reaction network (MRN).
#'
#' This function integrates feature-level statistical scores with a
#' metabolic reaction network and identifies a robust sub-network
#' through repeated PCSF optimization. Optional grid tuning of
#' parameters `b` and `w` is supported.
#'
#' @param res A \code{featuremap_result} object returned by
#'   \code{\link{annotate_feature}} (or a later stage). It must contain at
#'   least \code{feature_table}, \code{anno_feature_table1}, and \code{refdata},
#'   and should be at stage \code{"feature_annotated"} or later.
#' @param feature Character. Feature selection mode:
#'   \itemize{
#'   \item \code{"all"}: use all rows in \code{res$feature_table}
#'   \item \code{"sig"}: use only rows with \code{pvalue < pcutoff}
#'   }
#' @param pcutoff Numeric. Significance threshold used only when
#'   \code{feature = "sig"} to filter \code{res$feature_table$pvalue}.
#' @param mrn An \code{igraph} object for the metabolic reaction network.
#'   If \code{NULL}, shiny loads the built-in
#'   \code{metabolic_reaction_network.rda}.
#' @param workers Integer. Number of parallel workers used in repeated PCSF runs.
#' @param times Integer. Number of PCSF repeats for final subnetwork construction.
#'   Larger values usually improve robustness but increase runtime.
#' @param b Numeric. Prize scaling factor for terminal nodes in PCSF.
#' @param mu Numeric. Hub penalty coefficient. Larger values penalize high-degree
#'   nodes more strongly.
#' @param w Numeric. Dummy edge cost in PCSF.
#' @param r Numeric. Random perturbation magnitude applied to edge weights across
#'   repeats (robustness resampling factor).
#' @param grid_tune Logical. Whether to tune \code{b} and \code{w} on a grid
#'   before the final run.
#' @param b_grid Numeric vector of candidate \code{b} values when
#'   \code{grid_tune = TRUE}.
#' @param w_grid Numeric vector of candidate \code{w} values when
#'   \code{grid_tune = TRUE}.
#' @param times_tune Integer. Number of repeats per \code{(b, w)} pair during
#'   grid tuning.
#'
#' @return The input \code{res} object updated in-place with:
#' \itemize{
#'   \item \code{feature_table}: with derived \code{importance} score columns
#'   \item \code{pcsf_subnet}: final PCSF-derived subnetwork (\code{igraph})
#'   \item \code{node_frequency}: node appearance frequency across repeats
#'   \item \code{edge_frequency}: edge appearance frequency across repeats
#'   \item \code{anno_feature_table2}: network-refined feature-metabolite table
#'   \item \code{tuning_grid} and \code{tuning_best}: (if \code{grid_tune = TRUE})
#'         tuning summary
#'   \item \code{stage}: updated to \code{"subnetwork_extracted"}
#' }
#'
#' @details
#' Feature importance scores are derived from effects value and p-value.
#' The PCSF algorithm is executed multiple times with randomized edge
#' perturbation to ensure robustness. Final subnetworks are constructed
#' based on node and edge frequency across runs.
#'
#' @examples
#' \dontrun{
#' res <- extract_subnetwork(
#'   res = res,
#'   feature = "sig",
#'   pcutoff = 0.05,
#'   mrn = NULL,
#'   workers = 4,
#'   times = 100,
#'   b = 40,
#'   mu = 1e-4,
#'   w = 35,
#'   r = 0.30,
#'   grid_tune = FALSE
#' )
#' }
#'
#' @export
extract_subnetwork = function(res = NULL,
                              feature = c("all", "sig"),
                              pcutoff = 0.05,
                              mrn = NULL,
                              workers = 4,
                              times = 100,
                              b = 40,
                              mu = 1e-4,
                              w = 30,
                              r = 0.30,
                              grid_tune = FALSE,
                              b_grid = seq(20, 40, by = 5),
                              w_grid = seq(25, 40, by = 5),
                              times_tune = 10){
  notify_step <- function(type = c("info", "success"), text) {
    type <- match.arg(type)
    if (requireNamespace("cli", quietly = TRUE)) {
      if (type == "info") {
        cli::cli_alert_info(text)
      } else {
        cli::cli_alert_success(text)
      }
    } else {
      message(text)
    }
  }

  notify_title <- function(text) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_h2(text)
    } else {
      message("\n", text)
    }
  }

  load_bundled_mrn <- function() {
    candidate_paths <- c(
      system.file("app/www/metabolic_reaction_network.rda", package = "featuremapshiny"),
      system.file("www/metabolic_reaction_network.rda", package = "featuremapshiny"),
      file.path("inst", "app", "www", "metabolic_reaction_network.rda"),
      file.path("app", "www", "metabolic_reaction_network.rda"),
      file.path("www", "metabolic_reaction_network.rda")
    )
    candidate_paths <- unique(candidate_paths[nzchar(candidate_paths)])
    candidate_paths <- candidate_paths[file.exists(candidate_paths)]

    if (length(candidate_paths) == 0) {
      stop("Cannot find bundled metabolic reaction network 'metabolic_reaction_network.rda'.")
    }

    e_mrn <- new.env(parent = emptyenv())
    load(candidate_paths[1], envir = e_mrn)
    e_mrn[[ls(e_mrn)[1]]]
  }

  feature <- match.arg(feature)

  if (is.null(res)){
    stop("[Error] res cannot be NULL.")
  }
  res <- ensure_featuremap_object(
    res,
    expected_stages = c("feature_annotated", "subnetwork_extracted", "module_identified", "module_annotated")
  )
  if (!"feature_table" %in% names(res)){
    stop("[Error] res$feature_table is missing.")
  }

  notify_title("Step 2: Subnetwork Extraction")
  has_user_mrn <- !is.null(mrn)
  start_time <- Sys.time()
  notify_step("info", "[1/3] Loading the metabolic reaction network.")
  if (is.null(mrn)){
    mrn <- load_bundled_mrn()
  }
  if (has_user_mrn) {
    notify_step("success", "Using a user-supplied metabolic reaction network.")
  } else {
    notify_step("success", "Loaded built-in metabolic reaction network.")
  }

  res$mrn = mrn
  res$feature_table %>%
    dplyr::mutate(
      score_fc = {
        fc <- abs(condition)
        (fc - min(fc, na.rm = TRUE)) /
          (max(fc, na.rm = TRUE) - min(fc, na.rm = TRUE))
      },
      score_p = -log10(pvalue),
      importance = sqrt(score_fc * score_p)
    ) %>%
    as.data.frame() -> res$feature_table


  if(feature == "all"){
    feature_sig <- res$feature_table
  }else if(feature == "sig"){
    feature_sig <- res$feature_table %>%
      dplyr::filter(pvalue < pcutoff)
    if (nrow(feature_sig) == 0)
      stop("[Error] No significant features found under pcutoff = ", pcutoff)
  }
  notify_step("info", "[2/3] Preparing subnetwork extraction.")

  anno_feature_table1_sub <- res$anno_feature_table1 %>%
    dplyr::filter(variable_id %in% feature_sig$variable_id) %>%
    dplyr::mutate(annotation_row_id = dplyr::row_number()) %>%
    as.data.frame()

  anno_feature_table1_sub %>%
    dplyr::select(
      annotation_row_id,
      variable_id,
      Compound.name,
      HMDB.ID,
      KEGG.ID,
      score_norm
    ) -> anno_FM

  mrn_nodes <- mrn %>%
    tidygraph::activate(nodes) %>%
    as.data.frame()

  mrn_compound_lookup <- mrn_nodes %>%
    dplyr::filter(!is.na(INCHI_ID), INCHI_ID != "", INCHI_ID != "<NA>") %>%
    dplyr::select(INCHI_ID, Compound_name) %>%
    dplyr::distinct(INCHI_ID, .keep_all = TRUE)

  mrnref_hmdb <- mrn_nodes %>%
    dplyr::filter(!is.na(HMDB_ID), HMDB_ID != "", HMDB_ID != "<NA>") %>%
    dplyr::select(HMDB_ID, INCHI_ID, Compound_name) %>%
    dplyr::distinct(HMDB_ID, .keep_all = TRUE)

  mrnref_kegg <- mrn_nodes %>%
    dplyr::filter(!is.na(KEGG_ID), KEGG_ID != "", KEGG_ID != "<NA>") %>%
    dplyr::select(KEGG_ID, INCHI_ID, Compound_name) %>%
    dplyr::distinct(KEGG_ID, .keep_all = TRUE)

  suppressWarnings({
    anno_hmdb <- anno_FM %>%
      dplyr::filter(!is.na(HMDB.ID), HMDB.ID != "") %>%
      dplyr::left_join(mrnref_hmdb, by = c("HMDB.ID" = "HMDB_ID")) %>%
      dplyr::filter(!is.na(INCHI_ID)) %>%
      dplyr::arrange(annotation_row_id, INCHI_ID) %>%
      dplyr::distinct(annotation_row_id, .keep_all = TRUE) %>%
      dplyr::mutate(mapping_source = "HMDB")

    hmdb_mapped_rows <- anno_hmdb$annotation_row_id

    anno_kegg <- anno_FM %>%
      dplyr::filter(!annotation_row_id %in% hmdb_mapped_rows) %>%
      dplyr::filter(!is.na(KEGG.ID), KEGG.ID != "") %>%
      dplyr::left_join(mrnref_kegg, by = c("KEGG.ID" = "KEGG_ID")) %>%
      dplyr::filter(!is.na(INCHI_ID)) %>%
      dplyr::arrange(annotation_row_id, INCHI_ID) %>%
      dplyr::distinct(annotation_row_id, .keep_all = TRUE) %>%
      dplyr::mutate(mapping_source = "KEGG")
  })

  anno_mapped <- dplyr::bind_rows(anno_hmdb, anno_kegg) %>%
    dplyr::arrange(variable_id, dplyr::desc(score_norm), mapping_source, INCHI_ID)

  notify_step(
    "success",
    paste0(
      "Using ", nrow(feature_sig), " features with ",
      nrow(anno_mapped), " mapped annotation rows for subnetwork extraction."
    )
  )

  c_min = 0.02
  FMpair <- anno_mapped %>%
    dplyr::mutate(cost = 1 - score_norm + c_min) %>%
    dplyr::select(variable_id, INCHI_ID, cost) %>%
    dplyr::distinct(variable_id, INCHI_ID, .keep_all = TRUE) %>%
    purrr::set_names(c("from", "to", "weight"))

  if (nrow(FMpair) == 0){
    stop("[Error] No matched feature-metabolite pairs (FMpair); check annotation table or ID format.")
  }

  mrn %>%
    tidygraph::activate(edges) %>%
    as.data.frame() %>%
    dplyr::select(INCHI_From, INCHI_To) %>%
    purrr::set_names(c("from", "to")) %>%
    dplyr::mutate(weight = min(max(FMpair$weight), 0.99)) -> MMpair

  imrn = igraph::graph_from_data_frame(dplyr::bind_rows(FMpair, MMpair), directed = FALSE)

  terminals <- feature_sig %>%
    dplyr::filter(variable_id %in% FMpair$from) %>%
    dplyr::select(variable_id, importance) %>%
    tibble::deframe()

  node_names = igraph::V(imrn)$name
  node_prz <- tibble::tibble(name = node_names) %>%
    dplyr::left_join(
      tibble::enframe(terminals, name = "name", value = "prize"),
      by = "name"
    ) %>%
    tidyr::replace_na(list(prize = 0)) %>%
    dplyr::pull(prize)

  hub_penalty <- tibble::tibble(
    name = node_names,
    degree = igraph::degree(imrn)
  ) %>%
    dplyr::mutate(penalty = -mu * sqrt(degree)) %>%
    dplyr::pull(penalty)

  dummies = names(terminals)
  edges_df <- igraph::ends(imrn, es = igraph::E(imrn)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    rlang::set_names(c("from", "to"))
  final_edges <- dplyr::bind_rows(
    tibble::tibble(from = "DUMMY", to = dummies),
    edges_df
  )
  from <- final_edges$from
  to <- final_edges$to

  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  run_pcsf_once <- function(b_val, w_val, times_val){

    node_prizes <- tibble::tibble(
      name = node_names,
      base_prize = b_val * node_prz,
      penalty = hub_penalty
    ) %>%
      dplyr::mutate(final_prize = dplyr::if_else(base_prize == 0, penalty, base_prize)) %>%
      dplyr::pull(final_prize)

    if(workers > 1){
      results_list <- furrr::future_map(
        1:times_val,
        function(i) {
          cost <- c(
            rep(w_val, length(dummies)),
            igraph::E(imrn)$weight + igraph::E(imrn)$weight *
              stats::runif(length(igraph::E(imrn)), 0, max(r, stats::sd(igraph::E(imrn)$weight)))
          )
          output <- PCSF2::call_sr(from, to, cost, node_names, node_prizes)
          edge <- data.frame(
            source = as.character(output[[1]]),
            target = as.character(output[[2]]),
            stringsAsFactors = FALSE
          )
          edge <- edge[edge$source != "DUMMY" & edge$target != "DUMMY", ]

          if (nrow(edge) > 0) {
            g <- igraph::graph_from_data_frame(edge, directed = FALSE)
            nodes <- igraph::V(g)$name
            return(list(graph = g, nodes = nodes, iter = i))
          } else {
            return(list(graph = NULL, nodes = NULL, iter = i))
          }
        },
        .options = furrr::furrr_options(
          seed = NULL,
          packages = c("igraph", "dplyr", "tibble", "tidyr"),
          globals = list(
            imrn = imrn, dummies = dummies, from = from, to = to, r = r,
            node_names = node_names, node_prz = node_prz, hub_penalty = hub_penalty
          )
        ),
        .progress = TRUE
      )
    }else{
      results_list <- purrr::map(
        1:times_val,
        function(i) {
          cost <- c(
            rep(w_val, length(dummies)),
            igraph::E(imrn)$weight + igraph::E(imrn)$weight *
              stats::runif(length(igraph::E(imrn)), 0, max(r, stats::sd(igraph::E(imrn)$weight)))
          )
          output <- PCSF2::call_sr(from, to, cost, node_names, node_prizes)
          edge <- data.frame(
            source = as.character(output[[1]]),
            target = as.character(output[[2]]),
            stringsAsFactors = FALSE
          )
          edge <- edge[edge$source != "DUMMY" & edge$target != "DUMMY", ]

          if (nrow(edge) > 0) {
            g <- igraph::graph_from_data_frame(edge, directed = FALSE)
            nodes <- igraph::V(g)$name
            return(list(graph = g, nodes = nodes, iter = i))
          } else {
            return(list(graph = NULL, nodes = NULL, iter = i))
          }
        }
      )
    }

    all_nodes <- unlist(lapply(results_list, function(x) x$nodes))
    graphs_list <- lapply(results_list, function(x) x$graph)
    names(graphs_list) <- paste0("graph_", seq_along(graphs_list))

    if (length(all_nodes) == 0) {
      stop("No nodes found across runs (all PCSF outputs empty).")
    }

    node_freq <- table(all_nodes) %>% sort(decreasing = TRUE)
    node_frequency <- tibble::tibble(
      name = names(node_freq),
      freq = as.numeric(node_freq)/times_val
    ) %>%
      dplyr::mutate(type = dplyr::case_when(
        stringr::str_detect(name, "InChI") ~ "metabolite",
        TRUE ~ "feature"
      ))

    all_edges <- purrr::map_dfr(seq_along(graphs_list), function(i) {
      g <- graphs_list[[i]]
      if (is.null(g) || igraph::gsize(g) == 0) {
        return(tibble::tibble(from = character(), to = character(), run = integer()))
      }
      igraph::ends(g, es = igraph::E(g)) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        rlang::set_names(c("from", "to")) %>%
        dplyr::mutate(run = i)
    })

    edge_frequency <- all_edges %>%
      dplyr::mutate(edge = dplyr::if_else(from < to,
                                          paste(from, to, sep = "|"),
                                          paste(to, from, sep = "|"))) %>%
      dplyr::group_by(edge) %>%
      dplyr::tally(name = "freq") %>%
      tidyr::separate(edge, into = c("from", "to"), sep = "\\|") %>%
      dplyr::mutate(freq = freq/times_val)

    if (nrow(edge_frequency) == 0) {
      stop("No PCSF-based sub-network could be identified under current (b,w).")
    }

    pcsf_subnet <- igraph::graph_from_data_frame(edge_frequency, directed = FALSE)
    igraph::E(pcsf_subnet)$weight <- edge_frequency$freq

    pcsf_subnet <- pcsf_subnet %>%
      igraph::set_vertex_attr("prize", value = node_frequency$freq[match(igraph::V(.)$name, node_frequency$name)]) %>%
      igraph::set_vertex_attr("type", value = "Steiner") %>%
      igraph::set_vertex_attr(
        "compound_name",
        value = mrn_compound_lookup$Compound_name[match(igraph::V(.)$name, mrn_compound_lookup$INCHI_ID)]
      )

    term_idx <- match(names(terminals), igraph::V(pcsf_subnet)$name)
    term_idx <- term_idx[!is.na(term_idx)]
    igraph::V(pcsf_subnet)$type[term_idx] <- "Terminal"
    class(pcsf_subnet) <- c("PCSF", "igraph")

    feature_nodes <- igraph::V(pcsf_subnet)[igraph::V(pcsf_subnet)$type == "Terminal"]$name

    feature2metabolite <- list()
    for (f in feature_nodes) {
      neigh <- igraph::neighbors(pcsf_subnet, f, mode = "all")$name
      feature2metabolite[[f]] <- neigh
    }

    feature_metabolite_df <- data.frame(
      feature = rep(names(feature2metabolite), lengths(feature2metabolite)),
      metabolite = unlist(feature2metabolite)
    ) %>%
      dplyr::mutate(network_supported = TRUE) %>%
      dplyr::left_join(
        mrn_nodes %>%
          dplyr::select(INCHI_ID, Compound_name, HMDB_ID, KEGG_ID) %>%
          dplyr::distinct(INCHI_ID, .keep_all = TRUE),
        by = c("metabolite" = "INCHI_ID")
      )
    rownames(feature_metabolite_df) = NULL

    anno_feature_table2_extra <- anno_mapped %>%
      dplyr::rename(HMDB_ID = HMDB.ID, KEGG_ID = KEGG.ID) %>%
      dplyr::select(
        annotation_row_id,
        variable_id,
        INCHI_ID,
        score_norm,
        mapping_source
      ) %>%
      dplyr::left_join(
        feature_metabolite_df,
        by = c("variable_id" = "feature", "INCHI_ID" = "metabolite")
      ) %>%
      dplyr::filter(!is.na(network_supported) & network_supported) %>%
      dplyr::mutate(
        mapped_compound_name = Compound_name,
        mapped_hmdb_id = HMDB_ID,
        mapped_kegg_id = KEGG_ID
      ) %>%
      dplyr::select(
        annotation_row_id,
        variable_id,
        INCHI_ID,
        score_norm,
        mapping_source,
        network_supported,
        mapped_compound_name,
        mapped_hmdb_id,
        mapped_kegg_id
      ) %>%
      dplyr::arrange(variable_id, dplyr::desc(score_norm))

    from_is_inchi <- all(grepl("^InChI", edge_frequency$from))
    to_is_inchi   <- all(grepl("^InChI", edge_frequency$to))

    if(from_is_inchi){
      anno_feature_table2_extra <- anno_feature_table2_extra %>%
        dplyr::left_join(
          edge_frequency %>%
            dplyr::filter(to %in% anno_feature_table2_extra$variable_id),
          by = c("variable_id" = "to", "INCHI_ID" = "from")
        )
    }else if(to_is_inchi){
      anno_feature_table2_extra <- anno_feature_table2_extra %>%
        dplyr::left_join(
          edge_frequency %>%
            dplyr::filter(from %in% anno_feature_table2_extra$variable_id),
          by = c("variable_id" = "from", "INCHI_ID" = "to")
        )
    }

    anno_feature_table2_extra <- anno_feature_table2_extra %>%
      dplyr::select(
        annotation_row_id,
        variable_id,
        INCHI_ID,
        score_norm,
        mapping_source,
        network_supported,
        mapped_compound_name,
        mapped_hmdb_id,
        mapped_kegg_id,
        freq
      ) %>%
      dplyr::rename(edge_freq = freq) %>%
      dplyr::arrange(variable_id, dplyr::desc(score_norm), dplyr::desc(edge_freq))

    anno_feature_table2_extra <- anno_feature_table2_extra %>%
      dplyr::mutate(weighted_anno_score = 0.7*score_norm + 0.3*edge_freq) %>%
      dplyr::group_by(variable_id) %>%
      dplyr::mutate(confidence_rank = dplyr::dense_rank(dplyr::desc(weighted_anno_score))) %>%
      dplyr::ungroup()

    anno_feature_table2 <- anno_feature_table1_sub %>%
      dplyr::filter(annotation_row_id %in% anno_feature_table2_extra$annotation_row_id) %>%
      dplyr::left_join(anno_feature_table2_extra, by = c("annotation_row_id", "variable_id", "score_norm")) %>%
      dplyr::left_join(
        res$feature_table[, c("variable_id", "importance")] %>%
          dplyr::distinct(variable_id, .keep_all = TRUE),
        by = "variable_id"
      ) %>%
      dplyr::mutate(
        Compound.name = dplyr::coalesce(Compound.name, mapped_compound_name),
        HMDB.ID = dplyr::coalesce(HMDB.ID, mapped_hmdb_id),
        KEGG.ID = dplyr::coalesce(KEGG.ID, mapped_kegg_id)
      ) %>%
      dplyr::select(
        -annotation_row_id,
        -network_supported,
        -mapped_compound_name,
        -mapped_hmdb_id,
        -mapped_kegg_id,
        -mapping_source
      ) %>%
      dplyr::arrange(variable_id, dplyr::desc(weighted_anno_score), dplyr::desc(edge_freq))

    list(
      pcsf_subnet = pcsf_subnet,
      node_frequency = node_frequency,
      edge_frequency = edge_frequency,
      anno_feature_table2 = anno_feature_table2
    )
  }

  best_b <- b
  best_w <- w

  if (isTRUE(grid_tune)) {
    grid <- tidyr::expand_grid(b = b_grid, w = w_grid)
    grid$unique_variable_id <- NA_integer_
    grid$mean_weighted_score <- NA_real_

    for (i in seq_len(nrow(grid))) {
      bb <- grid$b[i]
      ww <- grid$w[i]

      out <- tryCatch(
        run_pcsf_once(b_val = bb, w_val = ww, times_val = times_tune),
        error = function(e) NULL
      )

      if (!is.null(out)) {
        grid$unique_variable_id[i] <- dplyr::n_distinct(out$anno_feature_table2$variable_id)
        grid$mean_weighted_score[i] <- mean(out$anno_feature_table2$weighted_anno_score, na.rm = TRUE)
      } else {
        grid$unique_variable_id[i] <- 0L
        grid$mean_weighted_score[i] <- NA_real_
      }
    }

    grid2 <- grid %>%
      dplyr::arrange(dplyr::desc(unique_variable_id), dplyr::desc(mean_weighted_score))

    best_b <- grid2$b[1]
    best_w <- grid2$w[1]

    res$tuning_grid <- grid2
    res$tuning_best <- list(
      b = best_b,
      w = best_w,
      unique_variable_id = grid2$unique_variable_id[1],
      mean_weighted_score = grid2$mean_weighted_score[1],
      times_tune = times_tune
    )

    notify_step(
      "success",
      paste0("Selected tuned PCSF parameters b = ", best_b, " and w = ", best_w, ".")
    )
  }


  notify_step("info", "[3/3] Finalizing network-supported annotations.")
  final_out <- run_pcsf_once(b_val = best_b, w_val = best_w, times_val = times)

  res$pcsf_subnet <- final_out$pcsf_subnet
  res$node_frequency <- final_out$node_frequency
  res$edge_frequency <- final_out$edge_frequency
  res$anno_feature_table2 <- final_out$anno_feature_table2

  res <- set_featuremap_stage(res, "subnetwork_extracted")

  end_time <- Sys.time()
  elapsed_min <- round(as.numeric(end_time - start_time, units = "mins"), 2)
  notify_step(
    "success",
    paste0(
      "Constructed a subnetwork with ", igraph::gorder(res$pcsf_subnet),
      " nodes and ", igraph::gsize(res$pcsf_subnet), " edges."
    )
  )
  notify_step(
    "success",
    paste0(
      "Retained ", nrow(res$anno_feature_table2), " network-supported annotation rows for ",
      dplyr::n_distinct(res$anno_feature_table2$variable_id), " features."
    )
  )
  notify_step("info", paste0("Elapsed time: ", elapsed_min, " minutes."))

  return(res)
}

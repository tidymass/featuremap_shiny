
#' Annotate raw features via MS1 or MS2 information
#'
#' Perform preliminary metabolite annotation for raw MS1 features
#' by matching against a reference metabolite database using mass
#' accuracy, retention time tolerance, isotope patterns, and optional
#' MS2 confirmation.
#'
#' @param feature_table A feature-level data frame to annotate. Must satisfy
#'   \code{\link{check_feature_table}} requirements, including columns:
#'   \code{variable_id}, \code{mz}, \code{rt}, \code{polarity},
#'   \code{mean_intensity}, \code{condition}, and \code{pvalue}.
#' @param ms2_table Optional MS2 confirmation table. Can be \code{NULL}.
#'   If provided, it should contain \code{variable_id} and at least one
#'   identifier column among \code{HMDB.ID} and \code{KEGG.ID}.
#' @param metabolite_database A \code{metid} database object used for MS1
#'   annotation. If \code{NULL}, shiny loads the built-in merged reference
#'   database \code{featuremap_compound.rda}.
#' @param refdata Deprecated compatibility argument retained for existing shiny
#'   callers. It is ignored and the returned object always stores
#'   \code{refdata = "all"} for the merged database.
#' @param column Character. LC column mode used by \code{metid} adduct/matching
#'   rules. Must be one of \code{"rp"} or \code{"hilic"}.
#' @param ms1_match_ppm Numeric. MS1 mass tolerance (ppm) used in candidate
#'   matching.
#' @param mfc_rt_tol Numeric. Retention-time tolerance (same unit as
#'   \code{feature_table$rt}) used in metabolite-feature-cluster scoring
#'   and isotope consistency checks.
#' @param isotope_number Integer. Maximum isotope depth used in isotope
#'   annotation.
#' @param workers Integer. Number of workers for parallel steps (isotope
#'   annotation) and threads passed to \code{metid::annotate_metabolites()}.
#'
#' @return A \code{featuremap_result} R6 object containing at least:
#'   \itemize{
#'     \item \code{feature_table}: input feature table
#'     \item \code{refdata}: fixed to \code{"all"} for the merged annotation database
#'     \item \code{anno_feature_table1}: feature-metabolite annotation table after redundancy removal
#'     \item \code{anno_redundancy}: redundancy summary before/after filtering
#'     \item \code{object_type}: \code{"featuremap_result"}
#'     \item \code{stage}: \code{"feature_annotated"}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' annotate_feature(
#'   feature_table = feature_table,
#'   ms2_table = NULL,
#'   metabolite_database = NULL
#' )
#' }
annotate_feature <- function(feature_table,
                             ms2_table = NULL,
                             metabolite_database = NULL,
                             refdata = NULL,
                             column = c("rp", "hilic"),
                             ms1_match_ppm = 15,
                             mfc_rt_tol = 5,
                             isotope_number = 3,
                             workers = 4
){
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

  safe_set_featuremap_stage <- function(x, stage) {
    stage_setter <- get0("set_featuremap_stage", mode = "function", inherits = TRUE)
    if (is.function(stage_setter)) {
      return(stage_setter(x, stage))
    }

    x$stage <- stage
    x$object_type <- "featuremap_result"
    x
  }

  load_bundled_metabolite_database <- function() {
    candidate_paths <- c(
      system.file("app/www/featuremap_compound.rda", package = "featuremapshiny"),
      system.file("www/featuremap_compound.rda", package = "featuremapshiny"),
      file.path("inst", "app", "www", "featuremap_compound.rda"),
      file.path("app", "www", "featuremap_compound.rda"),
      file.path("www", "featuremap_compound.rda")
    )
    candidate_paths <- unique(candidate_paths[nzchar(candidate_paths)])
    candidate_paths <- candidate_paths[file.exists(candidate_paths)]

    if (length(candidate_paths) == 0) {
      stop(
        "Cannot find bundled annotation database 'featuremap_compound.rda'."
      )
    }

    e_ref <- new.env(parent = emptyenv())
    load(candidate_paths[1], envir = e_ref)
    e_ref[[ls(e_ref)[1]]]
  }

  column <- match.arg(column)
  start_time <- Sys.time()

  notify_title("Step 1: Feature Annotation")
  notify_step("info", "[1/3] Preparing annotation references.")
  if (is.null(metabolite_database)) {
    metabolite_database <- load_bundled_metabolite_database()
    notify_step("success", "Loaded built-in merged annotation reference: featuremap_compound.rda.")
  } else {
    notify_step("success", "Using user-supplied annotation references.")
  }

  res <- R6Class(
    "featuremap_result",
    public = list(initialize = function(...) {}),
    lock_objects = FALSE
  )$new()
  res <- safe_set_featuremap_stage(res, "initialized")

  res$feature_table <- feature_table
  res$refdata <- "all"

  notify_step("info", "[2/3] Annotating metabolite features.")
  res$anno_feature_table1 <-
    annotate_feature_table(
      feature_table = feature_table,
      ms2_table = ms2_table,
      column = column,
      metabolite_database = metabolite_database,
      ms1_match_ppm = ms1_match_ppm,
      mfc_rt_tol = mfc_rt_tol,
      isotope_number = isotope_number,
      workers = workers
    )

  notify_step(
    "success",
    paste0(
      "Prepared ", nrow(res$anno_feature_table1), " annotation rows for ",
      dplyr::n_distinct(res$anno_feature_table1$variable_id),
      " features before redundancy filtering."
    )
  )

  notify_step("info", "[3/3] Removing low-confidence redundant annotations.")
  rr1 <- calculate_redundancy(annotation_table = res$anno_feature_table1)

  res$anno_feature_table1 <-
    remove_redundancy(annotation_table = res$anno_feature_table1)

  rr2 <- calculate_redundancy(annotation_table = res$anno_feature_table1)

  res$anno_feature_table1 <-
    res$anno_feature_table1 %>%
    dplyr::mutate(score_norm = score / max(res$anno_feature_table1$score, na.rm = TRUE)) %>%
    dplyr::arrange(variable_id, dplyr::desc(score))

  res$anno_redundancy <- data.frame(
    mfc_per_metabolite = as.numeric(c(rr1[1], rr2[1])),
    metabolite_per_feature = as.numeric(c(rr1[2], rr2[2]))
  )

  rownames(res$anno_redundancy) <- c("before", "after")
  res <- safe_set_featuremap_stage(res, "feature_annotated")

  end_time <- Sys.time()
  elapsed_min <- round(as.numeric(end_time - start_time, units = "mins"), 2)
  notify_step(
    "success",
    paste0(
      "Retained ", nrow(res$anno_feature_table1), " annotation rows for ",
      dplyr::n_distinct(res$anno_feature_table1$variable_id),
      " features."
    )
  )
  notify_step("info", paste0("Elapsed time: ", elapsed_min, " minutes."))

  return(res)
}


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
#'   identifier column among \code{HMDB.ID} and \code{KEGG.ID}. It is used only
#'   as auxiliary evidence for MS1-derived feature-metabolite pairs: a pair is
#'   considered MS2-supported when \code{variable_id + HMDB.ID} or
#'   \code{variable_id + KEGG.ID} matches the MS2 table.
#' @param metabolite_database A \code{metid} database object (typically
#'   \code{databaseClass}) used for MS1 annotation. If \code{NULL},
#'   the function loads the bundled merged reference database
#'   \code{featuremap_compound.rda}.
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
#'     \item \code{feature_table}: input feature table (with derived fields added later in pipeline)
#'     \item \code{refdata}: fixed to \code{"all"} to indicate merged HMDB/KEGG annotation
#'     \item \code{anno_feature_table1}: feature-metabolite annotation table after redundancy removal
#'     \item \code{anno_redundancy}: redundancy summary before/after filtering
#'     \item \code{object_type}: \code{"featuremap_result"}
#'     \item \code{stage}: \code{"feature_annotated"}
#'   }
#'
#' @examples
#' \dontrun{
#' data("demo_feature_table", package = "featuremap")
#' data("demo_ms2", package = "featuremap")
#'
#' res <- annotate_feature(
#'   feature_table = demo_feature_table,
#'   ms2_table = demo_ms2,
#'   metabolite_database = NULL,
#'   column = "rp",
#'   ms1_match_ppm = 15,
#'   mfc_rt_tol = 5,
#'   isotope_number = 3,
#'   workers = 4
#' )
#' }
#'
#' @export
annotate_feature <- function(feature_table,
                             ms2_table = NULL,
                             metabolite_database = NULL,
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
  has_user_database <- !is.null(metabolite_database)
  start_time <- Sys.time()

  notify_title("Step 1: Feature Annotation")
  notify_step("info", "[1/3] Loading annotation references.")
  if (is.null(metabolite_database)) {
    metabolite_database <- load_bundled_metabolite_database()
  }
  if (has_user_database) {
    notify_step("success", "Using user-supplied annotation references.")
  } else {
    notify_step("success", "No user-supplied annotation references detected; loaded built-in resources.")
  }

  res <- R6Class(
    "featuremap_result",
    public = list(initialize = function(...) {}),
    lock_objects = FALSE
  )$new()
  res <- set_featuremap_stage(res, "initialized")

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
  res <- set_featuremap_stage(res, "feature_annotated")

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

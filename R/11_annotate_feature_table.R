#' Annotate Metabolite Features Using MS1 and MS2 Data
#'
#' Performs metabolite annotation for LC-MS feature tables using MS1 data, with optional integration of MS2-based annotations.
#' The function supports polarity-specific annotation, adduct handling, isotope identification, and redundant annotation reduction.
#'
#' @param feature_table A data frame with metabolite features. Must include columns:
#'   `variable_id`, `mz`, `rt`, `condition`, `polarity`, and `mean_intensity`.
#' @param column Character. Chromatographic column type: either `"rp"` (reverse-phase) or `"hilic"`. Default is `"rp"`.
#' @param metabolite_database A metabolite database object (from `metid`) used for annotation.
#' @param ms1_match_ppm Numeric. m/z tolerance (in ppm) for MS1 matching. Default is 15.
#' @param mfc_rt_tol Numeric. Retention time tolerance (in seconds) for metabolite feature cluster scoring. Default is 5.
#' @param isotope_number Integer. Maximum number of isotopes to annotate. Default is 3.
#'
#' @return A data frame containing the final annotated metabolite feature table. It includes matched annotations,
#'   isotope annotations, compound-level metadata, and scoring metrics.
#'
#' @details
#' The function includes the following steps:
#' \itemize{
#'   \item MS1-based metabolite annotation (positive and negative modes separately)
#'   \item Optional inclusion of MS2-based annotations
#'   \item Isotope matching and scoring
#'   \item Feature merging into metabolite feature clusters (MFCs)
#'   \item Annotation quality scoring and redundancy calculation
#' }
#'
#' Parallel computing is enabled for isotope annotation using `future` and `furrr` packages.
#'
#' @author Xiaotao Shen \email{xiaotao.shen@outlook.com}
#'
#' @examples
#' \dontrun{
#' data("example_feature_table")
#' data("example_metabolite_database")
#'
#' annotated <- annotate_feature_table(
#'   feature_table = example_feature_table,
#'   metabolite_database = example_metabolite_database,
#'   column = "rp",
#'   ms1_match_ppm = 15,
#'   mfc_rt_tol = 5
#' )
#' }
#'
#' @export

annotate_feature_table <-
  function(feature_table,
           ms2_table,
           column = c("rp", "hilic"),
           metabolite_database,
           refdata = NULL,
           ms1_match_ppm = 15,
           mfc_rt_tol = 5,
           isotope_number = 3,
           workers) {
    
    column = match.arg(column)

    combine_isotope_results <- function(results) {
      keep <- Filter(Negate(is.null), results)
      if (length(keep) == 0) {
        return(NULL)
      }

      out <- do.call(rbind, keep)
      if (is.null(out)) {
        return(NULL)
      }

      as.data.frame(out)
    }

    ####feature_table is required
    ###check feature_table
    if (missing(feature_table)) {
      stop("feature_table is required.")
    } else{
      check_feature_table(feature_table)
    }
    
    
    
    ##metabolite_database is required
    if (missing(metabolite_database)) {
      stop("metabolite_database is required.")
    }
    
    feature_table_pos <-
      feature_table %>%
      dplyr::filter(polarity == "positive")
    
    feature_table_neg <-
      feature_table %>%
      dplyr::filter(polarity == "negative")

    ############metabolite database matching for feature_table
    message("\nAnnotating features positive mode...\n")
    
    if (nrow(feature_table_pos) == 0) {
      annotation_table_ms1_pos <- NULL
    } else{
      expression_data <-
        data.frame(sample_1 = rep(1, nrow(feature_table_pos)))
      sample_info <-
        data.frame(sample_id = "sample_1", class = "Subject")
      rownames(expression_data) <- feature_table_pos$variable_id
      object_pos <-
        massdataset::create_mass_dataset(
          expression_data = expression_data,
          sample_info = sample_info,
          variable_info = feature_table_pos
        )
      
      object_pos <-
        metid::annotate_metabolites(
          object = object_pos,
          based_on = "ms1",
          ms1.match.ppm = ms1_match_ppm,
          column = column,
          polarity = "positive",
          adduct.table = featuremap_load_metid_adduct_table(
            polarity = "positive",
            column = column
          ),
          database = metabolite_database,
          candidate.num = 1000,
          threads = max(1L, as.integer(workers))
        )
      
      annotation_table_ms1_pos <-
        object_pos@annotation_table
      annotation_table_ms1_pos$polarity <- "positive"
    }
    
    message("\nAnnotating features negative mode...\n")
    
    if (nrow(feature_table_neg) == 0) {
      annotation_table_ms1_neg <- NULL
    } else{
      expression_data <-
        data.frame(sample_1 = rep(1, nrow(feature_table_neg)))
      sample_info = data.frame(sample_id = "sample_1", class = "Subject")
      rownames(expression_data) <- feature_table_neg$variable_id
      
      object_neg <-
        massdataset::create_mass_dataset(
          expression_data = expression_data,
          sample_info = sample_info,
          variable_info = feature_table_neg
        )
      
      object_neg <-
        metid::annotate_metabolites(
          object = object_neg,
          based_on = "ms1",
          ms1.match.ppm = ms1_match_ppm,
          column = column,
          polarity = "negative",
          adduct.table = featuremap_load_metid_adduct_table(
            polarity = "negative",
            column = column
          ),
          database = metabolite_database,
          candidate.num = 1000,
          threads = max(1L, as.integer(workers))
        )
      
      annotation_table_ms1_neg <-
        object_neg@annotation_table
      annotation_table_ms1_neg$polarity <- "negative"
    }
    
    if(refdata == "kegg"){
      annotation_table_ms1 <-
        rbind(annotation_table_ms1_pos, annotation_table_ms1_neg) %>%
        as.data.frame() %>%
        dplyr::filter(!is.na(KEGG.ID)) %>%
        dplyr::mutate(isotope = "[M]")
    }else if(refdata == "hmdb"){
      annotation_table_ms1 <-
        rbind(annotation_table_ms1_pos, annotation_table_ms1_neg) %>%
        as.data.frame() %>%
        dplyr::filter(!is.na(HMDB.ID)) %>%
        dplyr::mutate(isotope = "[M]")
    }
    
    
    annotation_table_final <-
      annotation_table_ms1 %>%
      dplyr::select(
        "variable_id",
        "Compound.name",
        "CAS.ID",
        "HMDB.ID",
        "KEGG.ID",
        "Lab.ID",
        "Adduct",
        "mz.error",
        "mz.match.score",
        "RT.error",
        "RT.match.score",
        "CE",
        "SS",
        "Total.score",
        "Database",
        "Level",
        "polarity",
        "isotope"
      )
    
    if(refdata == "kegg"){
      
      annotation_table_final <-
        annotation_table_final %>%
        dplyr::left_join(metabolite_database@spectra.info[, c("KEGG.ID", "Formula")], by = "KEGG.ID") %>%
        dplyr::filter(!is.na(KEGG.ID)) %>%
        dplyr::filter(KEGG.ID != "")
      
    }else if(refdata == "hmdb"){
      
      annotation_table_final <-
        annotation_table_final %>%
        dplyr::left_join(metabolite_database@spectra.info[, c("HMDB.ID", "Formula")], by = "HMDB.ID") %>%
        dplyr::filter(!is.na(HMDB.ID)) %>%
        dplyr::filter(HMDB.ID != "")
      
    }
    
    
    annotation_table_final <-
      annotation_table_final %>%
      dplyr::left_join(feature_table %>% dplyr::select(-polarity), by = "variable_id")
    
    ##------------------------------------------------------------------------------
    ####isotope annotation
    annotation_table_final_pos <-
      annotation_table_final %>%
      dplyr::filter(polarity == "positive")
    
    annotation_table_final_neg =
      annotation_table_final %>%
      dplyr::filter(polarity == "negative")
    
    ###positive
    
    future::plan(future::multisession, workers = workers)
    
    message("\nIsotope annotation for feature table positive mode ...\n")
    
    if (nrow(annotation_table_final_pos) == 0) {
      isotope_pos <- NULL
    } else{
      system.time(
        isotope_pos <-
          furrr::future_map(
            .x = 1:nrow(annotation_table_final_pos),
            .f = function(x) {
              tryCatch({
                adduct <-
                  stringr::str_extract(annotation_table_final_pos$Adduct[x], "\\(.+\\)") %>%
                  stringr::str_replace("\\(", "") %>%
                  stringr::str_replace("\\)", "")
                
                temp_iso <- annotate_isotope(
                  formula = annotation_table_final_pos$Formula[x],
                  adduct = adduct,
                  mz = annotation_table_final_pos$mz[x],
                  rt = annotation_table_final_pos$rt[x],
                  mean_intensity = annotation_table_final_pos$mean_intensity[x],
                  peak.mz = feature_table_pos$mz,
                  peak.rt = feature_table_pos$rt,
                  peak.int = feature_table_pos$mean_intensity,
                  rt.tol = mfc_rt_tol,
                  mz.tol = ms1_match_ppm,
                  int.tol = 0.3,
                  max.isotope = isotope_number
                )
                
                if (is.null(temp_iso)) {
                  return(NULL)
                }
                
                matched_feature_rows <-
                  feature_table_pos[temp_iso$peakIndex, , drop = FALSE]

                temp_iso_result <- matched_feature_rows
                temp_iso_result$mz.error <- temp_iso$mzError.ppm
                temp_iso_result$isotope <- temp_iso$isotopes
                temp_iso_result$RT.error <- temp_iso$`rtError.s`

                temp_iso_result$Compound.name <-
                  annotation_table_final_pos$Compound.name[x]
                temp_iso_result$Lab.ID <- annotation_table_final_pos$Lab.ID[x]
                temp_iso_result$Adduct <- annotation_table_final_pos$Adduct[x]
                temp_iso_result$Formula <- annotation_table_final_pos$Formula[x]
                temp_iso_result$CAS.ID <- annotation_table_final_pos$CAS.ID[x]
                temp_iso_result$HMDB.ID <- annotation_table_final_pos$HMDB.ID[x]
                temp_iso_result$KEGG.ID <- annotation_table_final_pos$KEGG.ID[x]
                temp_iso_result$Database <- annotation_table_final_pos$Database[x]
                temp_iso_result$SS <- annotation_table_final_pos$SS[x]
                temp_iso_result$Total.score <- annotation_table_final_pos$Total.score[x]
                temp_iso_result$Level <- annotation_table_final_pos$Level[x]
                temp_iso_result$CE <- annotation_table_final_pos$CE[x]
                
                temp_iso_result$mz.match.score <-
                  (ms1_match_ppm - temp_iso_result$mz.error) / ms1_match_ppm
                temp_iso_result$RT.match.score <-
                  (mfc_rt_tol - temp_iso_result$RT.error) / mfc_rt_tol

                missing_cols <- setdiff(
                  colnames(annotation_table_final_pos),
                  colnames(temp_iso_result)
                )
                if (length(missing_cols) > 0) {
                  for (col_name in missing_cols) {
                    temp_iso_result[[col_name]] <- NA
                  }
                }

                temp_iso_result %>%
                  dplyr::select(colnames(annotation_table_final_pos))
              }, error = function(e) {
                message(
                  "[isotope-positive] skipped index ", x,
                  " variable_id=", annotation_table_final_pos$variable_id[x],
                  " formula=", annotation_table_final_pos$Formula[x],
                  " adduct=", annotation_table_final_pos$Adduct[x],
                  " error=", conditionMessage(e)
                )
                NULL
              })
            },
            .progress = TRUE
          ) %>%
          combine_isotope_results()
      )
    }
    
    
    if (!is.null(isotope_pos) && nrow(isotope_pos) > 0) {
      annotation_table_final_pos <-
        rbind(annotation_table_final_pos, isotope_pos) %>%
        as.data.frame() %>%
        dplyr::arrange(Compound.name, isotope, rt)
    } else {
      annotation_table_final_pos <-
        annotation_table_final_pos %>%
        as.data.frame() %>%
        dplyr::arrange(Compound.name, isotope, rt)
    }
    
    message("\nIsotope annotation for feature table negative mode...\n")

    if (nrow(annotation_table_final_neg) == 0) {
      isotope_neg <- NULL
    } else {
      system.time(
        isotope_neg <-
          furrr::future_map(
            .x = 1:nrow(annotation_table_final_neg),
            .f = function(x) {
              tryCatch({
                adduct <-
                  stringr::str_extract(annotation_table_final_neg$Adduct[x], "\\(.+\\)") %>%
                  stringr::str_replace("\\(", "") %>%
                  stringr::str_replace("\\)", "")
                
                temp_iso <- annotate_isotope(
                  formula = annotation_table_final_neg$Formula[x],
                  adduct = adduct,
                  mz = annotation_table_final_neg$mz[x],
                  rt = annotation_table_final_neg$rt[x],
                  mean_intensity = annotation_table_final_neg$mean_intensity[x],
                  peak.mz = feature_table_neg$mz,
                  peak.rt = feature_table_neg$rt,
                  peak.int = feature_table_neg$mean_intensity,
                  rt.tol = mfc_rt_tol,
                  mz.tol = ms1_match_ppm,
                  int.tol = 0.3,
                  max.isotope = isotope_number
                )
                
                if (is.null(temp_iso)) {
                  return(NULL)
                }
                
                matched_feature_rows <-
                  feature_table_neg[temp_iso$peakIndex, , drop = FALSE]

                temp_iso_result <- matched_feature_rows
                temp_iso_result$mz.error <- temp_iso$mzError.ppm
                temp_iso_result$isotope <- temp_iso$isotopes
                temp_iso_result$RT.error <- temp_iso$`rtError.s`

                temp_iso_result$Compound.name <-
                  annotation_table_final_neg$Compound.name[x]
                temp_iso_result$Lab.ID <- annotation_table_final_neg$Lab.ID[x]
                temp_iso_result$Adduct <- annotation_table_final_neg$Adduct[x]
                temp_iso_result$Formula <- annotation_table_final_neg$Formula[x]
                temp_iso_result$CAS.ID <- annotation_table_final_neg$CAS.ID[x]
                temp_iso_result$HMDB.ID <- annotation_table_final_neg$HMDB.ID[x]
                temp_iso_result$KEGG.ID <- annotation_table_final_neg$KEGG.ID[x]
                temp_iso_result$Database <- annotation_table_final_neg$Database[x]
                temp_iso_result$SS <- annotation_table_final_neg$SS[x]
                temp_iso_result$Total.score <- annotation_table_final_neg$Total.score[x]
                temp_iso_result$Level <- annotation_table_final_neg$Level[x]
                temp_iso_result$CE <- annotation_table_final_neg$CE[x]
                
                temp_iso_result$mz.match.score <-
                  (ms1_match_ppm - temp_iso_result$mz.error) / ms1_match_ppm
                temp_iso_result$RT.match.score <-
                  (mfc_rt_tol - temp_iso_result$RT.error) / mfc_rt_tol

                missing_cols <- setdiff(
                  colnames(annotation_table_final_neg),
                  colnames(temp_iso_result)
                )
                if (length(missing_cols) > 0) {
                  for (col_name in missing_cols) {
                    temp_iso_result[[col_name]] <- NA
                  }
                }

                temp_iso_result %>%
                  dplyr::select(colnames(annotation_table_final_neg))
              }, error = function(e) {
                message(
                  "[isotope-negative] skipped index ", x,
                  " variable_id=", annotation_table_final_neg$variable_id[x],
                  " formula=", annotation_table_final_neg$Formula[x],
                  " adduct=", annotation_table_final_neg$Adduct[x],
                  " error=", conditionMessage(e)
                )
                NULL
              })
            },
            .progress = TRUE
          ) %>%
          combine_isotope_results()
      )
    }
    
    if (!is.null(isotope_neg) && nrow(isotope_neg) > 0) {
      annotation_table_final_neg <-
        rbind(annotation_table_final_neg, isotope_neg) %>%
        as.data.frame() %>%
        dplyr::arrange(Compound.name, isotope, rt)
    } else {
      annotation_table_final_neg <-
        annotation_table_final_neg %>%
        as.data.frame() %>%
        dplyr::arrange(Compound.name, isotope, rt)
    }
    
    annotation_table_final <-
      rbind(annotation_table_final_pos, annotation_table_final_neg) %>%
      dplyr::arrange(Lab.ID, rt)
    
    if(!is.null(ms2_table)){
      if(refdata == "hmdb"){
        annotation_table_final <-
          annotation_table_final %>%
          dplyr::left_join(ms2_table,by = c("variable_id", "HMDB.ID"),suffix = c("", ".ms2")) %>%
          dplyr::mutate(SS = coalesce(SS.ms2, SS),
                        Level = if_else(!is.na(SS), 2L, Level)) %>%
          dplyr::select(-SS.ms2)
      }else if(refdata == "kegg"){
        annotation_table_final <-
          annotation_table_final %>%
          dplyr::left_join(ms2_table,by = c("variable_id", "KEGG.ID"),suffix = c("", ".ms2")) %>%
          dplyr::mutate(SS = coalesce(SS.ms2, SS),
                        Level = if_else(!is.na(SS), 2L, Level)) %>%
          dplyr::select(-SS.ms2)
      }
    }
    
    ####combine peaks to metabolite feature cluster (MFC)
    message("\n\nScoring metabolite feature clusters...\n")
    
    annotation_table_final <-
      score_annotation_table(annotation_table = annotation_table_final, mfc_rt_tol = mfc_rt_tol)
    
    
    ###remove redundant annotation according to metabolite
    calculate_redundancy(annotation_table = annotation_table_final)
    message("\nAll done!\n")
    return(annotation_table_final)
  }




#' Score Metabolite Feature Clusters (MFCs) Based on RT Grouping
#'
#' Assigns metabolite feature clusters (MFCs) within each compound (`Lab.ID`) by grouping features based on retention time (RT)
#' and calculating a cluster-level score using `score_mfc()`. Each feature is tagged with a `metabolite_feature_cluster` identifier and a computed `score`.
#'
#' @param annotation_table A data frame containing annotated metabolite features. Must include at least the columns
#'   `Lab.ID` and `rt` (retention time).
#' @param mfc_rt_tol Numeric. Retention time tolerance (in seconds) used for grouping features into the same cluster. Default is 10.
#'
#' @return A data frame with an additional `metabolite_feature_cluster` column (unique cluster ID) and `score` column for each feature.
#'
#' @details
#' \itemize{
#'   \item Features are grouped by `Lab.ID`, then clustered into MFCs based on RT proximity.
#'   \item Each MFC is scored via the `score_mfc()` function (assumed to be user-defined).
#'   \item Parallel computation is used via `furrr::future_map()` for faster processing.
#' }
#'
#' @author Xiaotao Shen \email{xiaotao.shen@outlook.com}
#'
#' @examples
#' \dontrun{
#' data("example_annotation_table")
#' scored_table <- score_annotation_table(annotation_table = example_annotation_table, mfc_rt_tol = 5)
#' head(scored_table)
#' }
#'
#' @export

score_annotation_table <-
  function(annotation_table, mfc_rt_tol) {
    combine_scored_clusters <- function(results, template) {
      keep <- Filter(Negate(is.null), results)
      if (length(keep) == 0) {
        template$metabolite_feature_cluster <- character(0)
        template$score <- numeric(0)
        return(template)
      }

      out <- do.call(rbind, keep)
      if (is.null(out)) {
        template$metabolite_feature_cluster <- character(0)
        template$score <- numeric(0)
        return(template)
      }

      as.data.frame(out)
    }

    ##calculate the score for each metabolite feature cluster
    annotation_table <-
      unique(annotation_table$Lab.ID) %>%
      furrr::future_map(
        .f = function(temp_id) {
          tryCatch({
            x <- annotation_table %>%
              dplyr::filter(Lab.ID == temp_id)

            x <- x %>%
              dplyr::filter(!is.na(rt)) %>%
              dplyr::arrange(rt)

            if (nrow(x) == 0) {
              return(NULL)
            }

            rt_class <-
              group_peaks_rt(rt = x$rt, rt.tol = mfc_rt_tol) %>%
              dplyr::arrange(rt)

            if (nrow(rt_class) == 0) {
              return(NULL)
            }

            rt_class <- paste(x$Lab.ID[1], rt_class$class, sep = "@")

            x$metabolite_feature_cluster <-
              rt_class

            x <-
              unique(x$metabolite_feature_cluster) %>%
              purrr::map(function(y) {
                z = x[x$metabolite_feature_cluster == y, , drop = FALSE]
                score <- score_mfc(z)
                z$score <- score
                z
              }) %>%
              do.call(rbind, .) %>%
              as.data.frame()
            x
          }, error = function(e) {
            message(
              "[score-annotation] skipped Lab.ID=", temp_id,
              " error=", conditionMessage(e)
            )
            NULL
          })
        },
        .progress = TRUE
      ) %>%
      combine_scored_clusters(annotation_table[0, , drop = FALSE])

    annotation_table <-
      annotation_table %>%
      dplyr::arrange(metabolite_feature_cluster, rt)

    return(annotation_table)
  }


# Internal helper: load metid adduct tables without relying on metid's
# default dataset loader.
featuremap_load_metid_adduct_table <- function(polarity = c("positive", "negative"),
                                               column = c("rp", "hilic")) {
  polarity <- match.arg(polarity)
  column <- match.arg(column)

  data_name <- switch(
    paste(column, polarity, sep = "."),
    "rp.positive" = "rp.pos",
    "rp.negative" = "rp.neg",
    "hilic.positive" = "hilic.pos",
    "hilic.negative" = "hilic.neg",
    stop("[Error] Unsupported adduct table combination: ", column, "/", polarity)
  )

  e <- new.env(parent = emptyenv())
  utils::data(list = data_name, package = "metid", envir = e)

  if (!exists(data_name, envir = e, inherits = FALSE)) {
    stop(
      "[Error] Failed to load default adduct table '", data_name,
      "' from package 'metid'. Please reinstall metid and retry."
    )
  }

  get(data_name, envir = e, inherits = FALSE)
}

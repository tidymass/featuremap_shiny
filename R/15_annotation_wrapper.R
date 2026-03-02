
run_step1_annotation_safe <- function(feature_table,
                                      metabolite_database,
                                      refdata = "kegg",
                                      column = "rp",
                                      ms1_match_ppm = 15,
                                      mfc_rt_tol = 5,
                                      isotope_number = 3,
                                      workers = 4) {
  tryCatch({
    result <- annotate_feature(
      feature_table = feature_table,
      metabolite_database = metabolite_database,
      refdata = refdata,
      column = column,
      ms1_match_ppm = ms1_match_ppm,
      mfc_rt_tol = mfc_rt_tol,
      isotope_number = isotope_number,
      workers = workers
    )
    
    list(ok = TRUE, result = result, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, result = NULL, error = as.character(e$message))
  })
}
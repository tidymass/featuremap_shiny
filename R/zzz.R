utils::globalVariables(c(
  ".", ".N", ".module_order",
  "annotation_row_id",
  "Annotated_fraction", "Annotated_nodes",
  "base_prize", "bin",
  "class_fraction", "condition", "cost", "coverage",
  "Compound_ID", "Compound_name", "Compound.name",
  "edge", "edge_freq", "edges",
  "fdr", "final_prize", "freq", "from", "functional_summary",
  "HHI",
  "HMDB_ID", "HMDB.ID", "hmdb_ms2_match",
  "id", "importance", "Importance",
  "INCHI_From", "INCHI_ID", "INCHI_To", "isotope",
  "KEGG_ID", "KEGG.ID", "kegg_ms2_match",
  "Lab.ID", "Level",
  "mapped_compound_name", "mapped_hmdb_id", "mapped_kegg_id", "mapping_source",
  "mean_weighted_score", "metabolite", "metabolite_feature_cluster", "module",
  "module_id", "Module", "Module.y", "Module_score", "Module_size",
  "mzError.ppm",
  "N", "n_hit", "n_metabolites", "n_real", "name",
  "network_supported", "Node_freq", "nodes",
  "p", "pathway_name", "peakIndex", "penalty", "polarity", "prize", "pvalue",
  "Robustness", "rt", "rtError.s",
  "score", "score_fc", "score_norm", "score_p", "SS", "SS.ms2",
  "to", "top_compound_id", "top_label", "top_rank", "tuning_success",
  "type", "Type",
  "unique_variable_id", "Use_for_annotation",
  "variable_id",
  "weighted_anno_score", "weighted_score"
))

# Keep app-only runtime dependencies visible to R CMD check.
.app_runtime_symbols <- list(
  DT = DTOutput,
  bslib = bs_theme,
  htmlwidgets = saveWidget,
  openxlsx = createWorkbook,
  plotly = plotlyOutput,
  scales = rescale,
  shinyjs = useShinyjs,
  visNetwork = visNetwork
)

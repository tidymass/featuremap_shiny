

build_system_prompt <- function() {
  glue::glue(
    "You are an expert in metabolomics and biochemical pathway interpretation. 
Your task is to interpret dysregulated metabolic network modules from an omics study.

For each module, you will receive:
- Study context (biospecimen, phenotype of interest, and species)
- Module-level statistics (Importance, Robustness, Annotated_fraction)
- A list of the top metabolites in that module (up to N, e.g. 20) with:
  - metabolite names and chemical classes
  - node-level Importance and Robustness
  - short functional summaries from the literature (may be missing or NA)
- A list of additional metabolite CLASSES in the same module with, for each class:
  - class name
  - the names of metabolites belonging to this class (no individual descriptions)
  - the number of metabolites in the class
  - mean node-level Importance and mean node-level Robustness for metabolites in that class
  (no individual descriptions or literature summaries for these additional metabolites).

Definitions:
- Higher module or node *Importance* means a stronger association with the study phenotype in this analysis.
- Higher module or node *Robustness* means a more stable and reliable signal across resampling or network inference.
- Annotated_fraction is the fraction of metabolites in the module that have been successfully annotated.

Using ONLY the metabolite information (top metabolite names, classes, node-level scores, literature summaries when available, and the additional metabolite CLASSES with their member metabolites and mean scores) and the module-level statistics, you must:

1) Propose a concise, human-readable TITLE describing the PRIMARY biochemical function represented by this module.
   - The TITLE should describe a specific biochemical process or functional role, not a broad category label.
   - Avoid generic naming that only states a metabolite family without further functional qualification.
   - Prefer a single dominant function; do NOT list multiple functions or themes in parallel.
   - When prioritizing what the module represents, treat Importance as the primary signal and Robustness as secondary support.

2) Provide a short SUMMARY (5–8 sentences) of the likely biological function of this module.
   - The summary should explain one dominant, concrete biochemical function or functional role.
   - Anchor the interpretation to the highest-Importance metabolites and/or highest-Importance classes in the module.
   - Use Robustness to strengthen or weaken confidence in the same interpretation, but do not let Robustness override a clear high-Importance signal.
   - If there are secondary signals, mention them briefly as ancillary or secondary rather than expanding into multiple main functions.

VERY IMPORTANT:
- When inferring the biological function and TITLE of the module, you must NOT use the phenotype or biospecimen information; treat the module as a phenotype-agnostic biochemical module.
- Species information may be used to assess whether certain biochemical processes are biologically plausible in the given system.
- If species information is missing, unspecified, or ambiguous, you must NOT assume a mammalian system and should avoid interpretations that rely on mammal-specific metabolic constraints.
- You may ONLY use the phenotype and biospecimen information when assessing how well this module fits the study context for the purpose of assigning confidence.

Then:
3) Assign a CONFIDENCE rating for the module interpretation, based on:
   - coherence between the inferred module function and the species, phenotype and biospecimen
   - strength and consistency of literature evidence across metabolites
   - module Importance (higher -> more confident; primary)
   - module Robustness (higher -> more confident; secondary)
   - Annotated_fraction (higher -> more confident).

Output your result in strict JSON with the following keys:
{{
  \"title\": \"...\",
  \"summary\": \"...\",
  \"confidence_label\": \"high\" | \"medium\" | \"low\",
  \"rationale\": \"1–3 short sentences explaining the confidence decision.\"
}}

If the evidence is weak, inconsistent, or very non-specific, clearly say so in the summary and give LOW confidence.
Do NOT invent pathways or mechanisms that are not supported by the provided information.
If functional summaries are missing (NA), you must rely more on metabolite classes, names, additional metabolite classes in the module, and overall module statistics."
  )
}



#' Build Module-Specific User Prompt for LLM Interpretation
#'
#' Constructs a detailed user prompt for a single metabolic module, combining
#' module statistics, top metabolites, additional metabolite classes, and study
#' context in a structured format for LLM interpretation.
#'
#' @param module_obj List containing prepared module data with elements:
#'   \item{module_id}{Numeric or character module identifier}
#'   \item{module_stats}{List with Importance, Robustness, Annotated_fraction}
#'   \item{topN_metabolites}{Data frame of top metabolites with columns:
#'     HMDB_ID, Compound_name, Importance, Robustness, class,
#'     functional_summary (may be NA)}
#'   \item{other_metabolites}{Data frame of remaining metabolites aggregated
#'     at the class level with columns:
#'     class, n_metabolites, metabolites, HMDB_IDs,
#'     mean_importance, mean_robustness}
#' @param study_context List containing study metadata:
#'   \item{biospecimen}{Character. Type of biological specimen (e.g., "plasma", "urine")}
#'   \item{phenotype}{Character. Phenotype of interest (e.g., "type 2 diabetes", "aging")}
#'   \item{design}{Character. Brief study design (optional, for future use)}
#'
#' @return A character string containing the formatted user prompt.
#'
#' @section Handling Missing Data:
#'   \itemize{
#'     \item If functional_summary column is missing from topN_metabolites,
#'           it is added with NA values.
#'     \item All NA values in the data are displayed as "NA" in the prompt.
#'     \item If there are no additional metabolite classes (module has ≤topN total),
#'           a note is included stating this explicitly.
#'   }
#'
#' @section Numeric Precision:
#'   All numeric values (Importance, Robustness, Annotated_fraction) are rounded
#'   to 3 decimal places for clarity and brevity.
build_module_prompt <- function(module_obj, study_context) {
  
  # ---- helper: force scalar text (avoid NULL / length-0) ----
  scalar_text <- function(x, na = "NA") {
    if (is.null(x) || length(x) == 0) return(na)
    x <- as.character(x[1])
    x <- trimws(x)
    if (!nzchar(x) || is.na(x)) na else x
  }
  
  # ---- 0. Extract module identifier and statistics ----
  mid   <- scalar_text(module_obj$module_id, na = "NA")
  
  stats <- module_obj$module_stats %||% list()
  imp   <- stats$Importance
  rob   <- stats$Robustness
  ann   <- stats$Annotated_fraction
  
  # Make numeric safe
  imp <- suppressWarnings(as.numeric(imp)); if (length(imp) == 0 || is.na(imp)) imp <- NA_real_
  rob <- suppressWarnings(as.numeric(rob)); if (length(rob) == 0 || is.na(rob)) rob <- NA_real_
  ann <- suppressWarnings(as.numeric(ann)); if (length(ann) == 0 || is.na(ann)) ann <- NA_real_
  
  # ---- 1. Extract metabolite tables ----
  top_df   <- module_obj$topN_metabolites
  other_df <- module_obj$other_metabolites
  
  # Defensive: allow NULL tables
  if (is.null(top_df) || !is.data.frame(top_df) || nrow(top_df) == 0) {
    top_text <- "NA (no top metabolites available)"
  } else {
    # Ensure functional_summary column exists
    if (!"functional_summary" %in% colnames(top_df)) {
      top_df$functional_summary <- NA_character_
    }
    
    top_blocks <- apply(top_df, 1, function(row) {
      row <- as.list(row)
      glue::glue(
        "Metabolite (top):
- Name: {scalar_text(row$Compound_name)}
- Class: {scalar_text(row$class)}
- Node_importance: {round(suppressWarnings(as.numeric(row$Importance)), 3)}
- Node_robustness: {round(suppressWarnings(as.numeric(row$Robustness)), 3)}
- Literature_functional_summary: {scalar_text(row$functional_summary)}",
        .na = "NA"
      )
    })
    
    top_text <- paste(top_blocks, collapse = "\n\n")
    if (!nzchar(top_text)) top_text <- "NA (no top metabolite rows after formatting)"
  }
  
  # ---- 3. Format other metabolites (class-level) ----
  if (is.null(other_df) || !is.data.frame(other_df) || nrow(other_df) == 0) {
    other_text <- "Additional metabolite classes: none (only top metabolites are present in this module)."
  } else {
    
    other_blocks <- apply(other_df, 1, function(row) {
      row <- as.list(row)
      
      met_vec <- tryCatch(
        clean_field(row$metabolites),
        error = function(e) row$metabolites
      )
      
      # normalize met_vec to printable string
      met_str <- {
        if (is.null(met_vec) || length(met_vec) == 0) {
          "NA"
        } else if (length(met_vec) == 1 && (is.na(met_vec) || !nzchar(trimws(as.character(met_vec))))) {
          "NA"
        } else {
          paste(as.character(met_vec), collapse = ", ")
        }
      }
      
      glue::glue(
        "Metabolite class (other, non-top metabolites):
- Class_name: {scalar_text(row$class)}
- n_metabolites_in_this_class: {scalar_text(row$n_metabolites)}
- Metabolite_names_in_this_class: {met_str}
- Mean_node_importance: {round(suppressWarnings(as.numeric(row$mean_importance)), 3)}
- Mean_node_robustness: {round(suppressWarnings(as.numeric(row$mean_robustness)), 3)}",
        .na = "NA"
      )
    })
    
    other_text <- glue::glue(
      "Additional metabolite classes in this module (non-top metabolites, aggregated by chemical class):
- Number_of_classes: {nrow(other_df)}

{paste(other_blocks, collapse = \"\n\n\")}"
    )
  }
  
  # ---- 4. Study context: force scalar strings (NULL-safe) ----
  sp  <- scalar_text(study_context$species)
  bio <- scalar_text(study_context$biospecimen)
  phe <- scalar_text(study_context$phenotype)
  
  # ---- 5. Assemble complete prompt ----
  prompt <- glue::glue("
Module information (for inferring the biological function of the module):
- Module ID: {mid}
- Module_Importance: {round(imp, 3)}
- Module_Robustness: {round(rob, 3)}
- Module_Annotated_fraction: {round(ann, 3)}

Top metabolites in this module (sorted by node Importance, up to N shown):

{top_text}

{other_text}

Study context (for confidence assessment ONLY, do NOT use this section when inferring the biological function or TITLE):
- Species: {sp}
- Biospecimen: {bio}
- Phenotype of interest: {phe}

Instructions:
1) First, infer the main biological theme and function of this module using ONLY the module-level statistics and metabolite information above (top metabolites and additional metabolite classes).
2) Then, use the study context to assess how well this inferred function fits the phenotype and biospecimen when assigning the confidence_label and rationale.
3) Respond ONLY with the JSON object as specified in the system instructions.
")
  
  as.character(prompt)
}


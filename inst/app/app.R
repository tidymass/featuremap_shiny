library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(visNetwork)
library(bslib)
library(magrittr)

load_featuremap_functions <- function() {
  package_name <- "featuremapshiny"
  target_env <- parent.frame()
  candidate_roots <- unique(
    normalizePath(
      c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", "..")),
      winslash = "/",
      mustWork = FALSE
    )
  )
  source_root <- Filter(
    function(path) {
      file.exists(file.path(path, "DESCRIPTION")) &&
        file.exists(file.path(path, "inst", "app", "app.R"))
    },
    candidate_roots
  )

  if (length(source_root) > 0 && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(
      source_root[[1]],
      export_all = FALSE,
      helpers = FALSE,
      quiet = TRUE
    )
  }

  if (!(package_name %in% loadedNamespaces()) &&
      !requireNamespace(package_name, quietly = TRUE)) {
    stop(
      "The featuremapshiny package must be installed or loaded before launching this app.",
      call. = FALSE
    )
  }

  ns <- asNamespace(package_name)
  package_objects <- ls(ns, all.names = TRUE)
  package_functions <- package_objects[vapply(
    package_objects,
    function(name) is.function(get(name, envir = ns)),
    logical(1)
  )]
  package_functions <- setdiff(package_functions, "run_featuremap")

  for (name in package_functions) {
    assign(name, get(name, envir = ns), envir = target_env)
  }

  invisible(NULL)
}

load_featuremap_functions()

citation_text <- paste(
  "FeatureMAP: A Comprehensive Pipeline for Metabolite Feature-based",
  "Functional Module Identification and Annotation.",
  "Journal Name, Year;Volume:Pages."
)

build_info_icon <- function(content) {
  tags$span(
    class = "inline-info-wrap",
    tabindex = "0",
    tags$span(
      class = "inline-info-icon",
      icon("circle-info")
    ),
    tags$span(
      class = "inline-info-card",
      HTML(content)
    )
  )
}


# UI Definition=================================================================
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#667eea",
    base_font = font_google("Inter")
  ),
  
  # Custom CSS
  tags$head(
    
    tags$style(HTML("
    
    .empty-panel {
        min-height: 560px;
        border: 1px dashed #dee2e6;
        border-radius: 10px;
        background: #f8f9fa;
        color: #6c757d;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
        font-size: 15px;
      }
    
    .empty-panel-compact {
        min-height: 280px;
      }
    
    .empty-panel-fill {
        min-height: 0;
        height: 100%;
        width: 100%;
        flex: 1;
      }

    
    
      /* running */
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      .pulse-animation {
        animation: pulse 1.5s ease-in-out infinite;
      }
      
      /* Process modal */
      @keyframes fmModalPulse {
        0% {
          transform: scale(1);
          box-shadow: 0 0 0 0 rgba(98, 145, 211, 0.45);
        }
        70% {
          transform: scale(1.03);
          box-shadow: 0 0 0 14px rgba(98, 145, 211, 0);
        }
        100% {
          transform: scale(1);
          box-shadow: 0 0 0 0 rgba(98, 145, 211, 0);
        }
      }
      
      .fm-process-modal {
        text-align: center;
        padding: 20px 14px 10px 14px;
      }
      
      .modal.fm-process-modal-wrap {
        display: flex !important;
        align-items: center;
        justify-content: center;
        padding: 0 !important;
      }
      
      .modal.fm-process-modal-wrap .modal-dialog {
        margin: 0 !important;
      }
      
      .fm-modal-dialog {
        max-width: 760px !important;
        width: min(94vw, 760px) !important;
      }
      
      .fm-modal-content {
        border: none;
        border-radius: 24px;
        box-shadow: 0 28px 80px rgba(19, 36, 64, 0.30);
      }
      
      .fm-modal-content .modal-body {
        padding: 24px 30px 20px 30px;
      }
      
      .fm-modal-icon {
        width: 96px;
        height: 96px;
        border-radius: 50%;
        margin: 0 auto 18px auto;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 38px;
      }
      
      .fm-modal-icon.running {
        color: white;
        background: linear-gradient(135deg, #6e96cf 0%, #4f77af 100%);
        animation: fmModalPulse 1.4s ease-out infinite;
      }
      
      .fm-modal-icon.success {
        color: #238b4e;
        background: #eaf7f0;
        border: 1px solid #c8e8d5;
      }
      
      .fm-modal-icon.error {
        color: #c93a3a;
        background: #fdeeee;
        border: 1px solid #f6c2c2;
      }
      
      .fm-modal-title {
        font-size: 28px;
        font-weight: 700;
        color: #2c3e50;
        margin-bottom: 12px;
      }
      
      .fm-modal-text {
        font-size: 16px;
        color: #5f6b76;
        line-height: 1.6;
        min-height: 52px;
      }

      .inline-info-wrap {
        position: relative;
        display: inline-flex;
        align-items: center;
        margin-left: 6px;
      }

      .inline-info-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        color: #5b6f82;
        cursor: pointer;
      }

      .inline-info-wrap:hover .inline-info-icon,
      .inline-info-wrap:focus-within .inline-info-icon,
      .inline-info-wrap:focus .inline-info-icon {
        color: #2f5d8a;
        outline: none;
      }

      .inline-info-card {
        display: none;
        position: absolute;
        top: calc(100% + 8px);
        left: 0;
        z-index: 2200;
        width: 320px;
        padding: 10px 12px;
        border-radius: 10px;
        border: 1px solid #d7e1ec;
        background: #ffffff;
        box-shadow: 0 14px 30px rgba(28, 49, 74, 0.16);
        color: #415366;
        font-size: 12px;
        line-height: 1.5;
        white-space: normal;
      }

      .inline-info-card code {
        color: #1f4f82;
        background: #eef4fb;
        padding: 1px 4px;
        border-radius: 4px;
      }

      .inline-info-card a {
        color: #1f5e98;
        text-decoration: underline;
      }

      .inline-info-wrap:hover .inline-info-card,
      .inline-info-wrap:focus-within .inline-info-card,
      .inline-info-wrap:focus .inline-info-card,
      .inline-info-wrap.is-pinned .inline-info-card {
        display: block;
      }
      
      /* Keep parameter/result cards aligned in each analysis step (desktop) */
      @media (min-width: 992px) {
        .step-main-row {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
        }
        
        .step-main-row > [class*='col-'] {
          display: flex;
        }
        
        .step-main-row > [class*='col-'] > .card {
          width: 100%;
          display: flex;
          flex-direction: column;
        }
        
        .step-main-row > [class*='col-'] > .card > .card-body {
          flex: 1;
        }
        
        #network_page .step-main-row {
          align-items: flex-start;
        }

        #network_page .step-main-row > [class*='col-'] {
          display: block;
        }

        #network_page .step-main-row > [class*='col-'] > .card {
          display: block;
        }

        #network_page #network_result_card {
          display: flex;
          flex-direction: column;
        }

        #network_page #network_result_card > .card-body {
          flex: 1;
        }

        #network_page .card > .card-body {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        #network_page .card > .card-body > .tabbable {
          display: flex;
          flex-direction: column;
          flex: 1;
          min-height: 0;
        }

        #network_page .card > .card-body > .tabbable > .tab-content {
          display: flex;
          flex: 1;
          min-height: 0;
        }

        #network_page .card > .card-body > .tabbable > .tab-content > .tab-pane {
          flex: 1;
          min-width: 0;
        }

        #network_page .card > .card-body > .tabbable > .tab-content > .active {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        #network_page #pcsf_network_container {
          display: flex;
          flex: 1;
          min-height: 0;
        }

        #network_page .pcsf-network-shell {
          display: flex;
          flex-direction: column;
          gap: 8px;
          min-height: 0;
        }

        #network_page .pcsf-network-topbar {
          display: flex;
          gap: 12px;
          align-items: center;
          flex-wrap: wrap;
        }

        #network_page .pcsf-network-stage {
          display: flex;
          flex: 1;
          min-height: 0;
          width: 100%;
        }

        #network_page .pcsf-network-stage > .empty-panel-fill,
        #network_page .pcsf-network-stage > .shiny-vis-network {
          flex: 1;
          min-height: 100%;
          height: 100%;
          width: 100%;
        }

        #network_page .pcsf-selector-row {
          display: flex;
          gap: 10px;
          align-items: center;
          margin-bottom: 0;
          flex: 1 1 auto;
        }

        #network_page .pcsf-selector-item {
          flex: 0 1 320px;
          width: 320px;
          max-width: 100%;
        }

        #network_page .pcsf-selector-input {
          display: flex;
          align-items: flex-start;
          gap: 8px;
        }

        #network_page .pcsf-selector-field {
          flex: 1;
          min-width: 0;
        }

        #network_page .pcsf-selector-field .form-group,
        #network_page .pcsf-selector-field .selectize-control {
          margin-bottom: 0;
        }

        #network_page .pcsf-selector-input .inline-info-wrap {
          margin-left: 0;
          margin-top: 8px;
          flex: 0 0 auto;
        }

        #network_page .pcsf-linked-table-card {
          margin-top: 0;
          flex: 0 0 220px;
          min-height: 220px;
          display: flex;
          flex-direction: column;
          overflow: hidden;
        }

        #network_page .pcsf-linked-table-card hr {
          margin: 0 0 2px 0;
          flex: 0 0 auto;
        }

        #network_page .pcsf-linked-table-card .annotation-table-wrap {
          flex: 1 1 auto;
          min-height: 0;
          overflow: auto;
        }

        #network_page .pcsf-linked-table-card .dataTables_wrapper {
          height: 100%;
          display: flex;
          flex-direction: column;
        }

        #network_page .pcsf-linked-table-card .dataTables_scroll {
          flex: 1 1 auto;
          min-height: 0;
        }

        #network_page .pcsf-linked-table-card .dataTables_scrollBody {
          max-height: 150px !important;
        }

        #network_page #pcsf_network_legend {
          flex: 0 0 auto;
          margin-left: auto;
        }

        #network_page[data-sync-panels='ready'] #network_result_card {
          height: var(--network-panel-height);
        }

        #network_page[data-sync-panels='ready'] #network_result_card > .card-body {
          min-height: 0;
        }

        #network_page[data-sync-panels='ready'] #network_result_card > .card-body {
          overflow: hidden;
        }

        #network_page[data-sync-panels='ready'] #network_result_card > .card-body > .tabbable,
        #network_page[data-sync-panels='ready'] #network_result_card > .card-body > .tabbable > .tab-content {
          min-height: 0;
          height: 100%;
        }

        #network_page[data-sync-panels='ready'] #network_result_card > .card-body > .tabbable > .tab-content > .tab-pane,
        #network_page[data-sync-panels='ready'] #network_result_card > .card-body > .tabbable > .tab-content > .active {
          min-height: 0;
        }

        #network_page[data-sync-panels='ready'] #network_result_card .pcsf-network-shell {
          flex: 1;
          height: 100%;
          min-height: 0;
          overflow: hidden;
        }

        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_node_selectors,
        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_network_legend,
        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_tuning_summary,
        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_selected_annotation_ui {
          flex: 0 0 auto;
          min-height: 0;
        }

        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_network_container {
          display: flex;
          flex: 1;
          height: 100%;
          min-height: 0;
        }

        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_network_container > .pcsf-network-stage {
          flex: 1;
          min-height: 0;
        }

        #network_page[data-sync-panels='ready'] #network_result_card #pcsf_network_container .empty-panel-fill {
          min-height: 100%;
          height: 100%;
        }

        #network_page[data-sync-panels='ready'] #network_result_card .annotation-table-wrap,
        #network_page[data-sync-panels='ready'] #network_result_card .pcsf-linked-table-card {
          min-height: 0;
          overflow-y: auto;
          overflow-x: hidden;
        }

        /* Upload: keep left panel natural and lock right panel to the same height with internal scroll */
        #upload_page .step-main-row {
          align-items: flex-start;
        }

        #upload_page .step-main-row > [class*='col-'] {
          display: block;
        }

        #upload_page .step-main-row > [class*='col-'] > .card {
          width: 100%;
        }

        #upload_page .card > .card-body {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        #upload_page .card > .card-body > .tabbable {
          display: flex;
          flex-direction: column;
          flex: 1;
          min-height: 0;
        }

        #upload_page .card > .card-body > .tabbable > .tab-content {
          display: flex;
          flex: 1;
          min-height: 0;
        }

        #upload_page .card > .card-body > .tabbable > .tab-content > .tab-pane {
          flex: 1;
          min-width: 0;
        }

        #upload_page .card > .card-body > .tabbable > .tab-content > .active {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        #upload_page[data-sync-panels='ready'] #upload_result_card {
          height: var(--upload-panel-height);
        }

        #upload_page[data-sync-panels='ready'] #upload_result_card > .card-body {
          min-height: 0;
          overflow: hidden;
        }

        #upload_page .upload-preview-shell {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
          flex-direction: column;
        }

        #upload_page .upload-preview-shell > .tabbable {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
          flex-direction: column;
        }

        #upload_page .upload-preview-shell > .tabbable > .tab-content {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
        }

        #upload_page .upload-preview-shell > .tabbable > .tab-content > .tab-pane {
          flex: 1 1 auto;
          min-height: 0;
        }

        #upload_page .upload-preview-shell > .tabbable > .tab-content > .active {
          display: flex;
          flex-direction: column;
          min-height: 0;
          overflow: hidden;
        }

        #upload_page .upload-preview-table {
          flex: 1 1 auto;
          min-height: 0;
          overflow: auto;
        }

        #upload_page .upload-preview-table .dataTables_wrapper {
          flex: 1 1 auto;
          min-height: 0;
          width: 100%;
        }

        #upload_page .upload-status-wrap {
          flex: 0 0 auto;
        }
        
        /* Annotation: lock row height to the left panel and scroll inside the right panel */
        #annotation_page .step-main-row {
          align-items: flex-start;
        }

        #annotation_page .step-main-row > [class*='col-'] {
          display: block;
        }

        #annotation_page .step-main-row > [class*='col-'] > .card {
          width: 100%;
        }

        #annotation_page .card > .card-body {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }
        
        #annotation_page .card > .card-body > .tabbable {
          display: flex;
          flex-direction: column;
          flex: 1;
          min-height: 0;
        }
        
        #annotation_page .card > .card-body > .tabbable > .tab-content {
          display: flex;
          flex: 1;
          min-height: 0;
        }
        
        #annotation_page .card > .card-body > .tabbable > .tab-content > .tab-pane {
          flex: 1;
          min-width: 0;
        }
        
        #annotation_page .card > .card-body > .tabbable > .tab-content > .active {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }
        
        #annotation_page .annotation-summary-stack {
          display: flex;
          flex-direction: column;
          gap: 12px;
          flex: 1;
          min-height: 0;
          width: 100%;
          min-width: 0;
        }
        
        #annotation_page .annotation-summary-stack > .shiny-html-output {
          display: flex;
          flex: 1;
          min-height: 0;
          width: 100%;
          min-width: 0;
        }

        #annotation_page[data-sync-panels='ready'] #annotation_param_card,
        #annotation_page[data-sync-panels='ready'] #annotation_result_card {
          height: var(--annotation-panel-height);
        }

        #annotation_page[data-sync-panels='ready'] #annotation_param_card > .card-body,
        #annotation_page[data-sync-panels='ready'] #annotation_result_card > .card-body {
          min-height: 0;
        }

        #annotation_page[data-sync-panels='ready'] #annotation_result_card > .card-body {
          overflow: hidden;
        }

        #annotation_page[data-sync-panels='ready'] #annotation_result_card > .card-body > .tabbable,
        #annotation_page[data-sync-panels='ready'] #annotation_result_card > .card-body > .tabbable > .tab-content {
          min-height: 0;
          height: 100%;
        }

        #annotation_page[data-sync-panels='ready'] #annotation_result_card > .card-body > .tabbable > .tab-content > .tab-pane,
        #annotation_page[data-sync-panels='ready'] #annotation_result_card > .card-body > .tabbable > .tab-content > .active,
        #annotation_page[data-sync-panels='ready'] #annotation_result_card .annotation-summary-stack {
          min-height: 0;
          overflow-y: auto;
          overflow-x: hidden;
        }
        
        /* Modules */
        #modules_page .step-main-row {
          align-items: flex-start;
        }

        #modules_page .step-main-row > [class*='col-'] {
          display: block;
        }

        #modules_page .step-main-row > [class*='col-'] > .card {
          width: 100%;
        }

        #modules_page .card > .card-body {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }
        
        #modules_page .card > .card-body > .tabbable {
          display: flex;
          flex-direction: column;
          flex: 1;
          min-height: 0;
        }
        
        #modules_page .card > .card-body > .tabbable > .tab-content {
          display: flex;
          flex: 1;
          min-height: 0;
        }
        
        #modules_page .card > .card-body > .tabbable > .tab-content > .tab-pane {
          flex: 1;
          min-width: 0;
        }
        
        #modules_page .card > .card-body > .tabbable > .tab-content > .active {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        #modules_page[data-sync-panels='ready'] #modules_result_card {
          height: var(--modules-panel-height);
        }

        #modules_page[data-sync-panels='ready'] #modules_result_card > .card-body {
          min-height: 0;
          overflow: hidden;
        }

        #modules_page[data-sync-panels='ready'] #modules_result_card > .card-body > .tabbable,
        #modules_page[data-sync-panels='ready'] #modules_result_card > .card-body > .tabbable > .tab-content {
          min-height: 0;
          height: 100%;
        }

        #modules_page[data-sync-panels='ready'] #modules_result_card > .card-body > .tabbable > .tab-content > .tab-pane,
        #modules_page[data-sync-panels='ready'] #modules_result_card > .card-body > .tabbable > .tab-content > .active {
          min-height: 0;
        }

        #modules_page .module-visual-shell,
        #modules_page .module-table-shell {
          display: flex;
          flex-direction: column;
          flex: 1 1 auto;
          min-height: 0;
        }

        #modules_page .module-visual-filter-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
          flex-wrap: nowrap;
          margin-bottom: 8px;
          width: 100%;
        }

        #modules_page .module-visual-shell > #module_selector_ui {
          flex: 0 0 auto;
          width: 100%;
          margin-bottom: 8px;
        }

        #modules_page .module-visual-shell > #module_selector_ui .module-selector-wrap {
          width: 100%;
        }

        #modules_page .module-visual-shell > #module_selector_ui .form-group {
          margin-bottom: 0;
        }

        #modules_page .module-selector-divider {
          margin: 0 0 8px 0;
          flex: 0 0 auto;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui {
          flex: 0 1 320px;
          width: 320px;
          max-width: 100%;
          min-width: 0;
          display: block;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-item {
          width: 100%;
          max-width: 100%;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-input {
          display: flex;
          align-items: flex-start;
          gap: 8px;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-field {
          flex: 1;
          min-width: 0;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-field .form-group,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-field .selectize-control {
          margin-bottom: 0;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-input .inline-info-wrap {
          margin-left: 0;
          margin-top: 8px;
          flex: 0 0 auto;
        }

        #modules_page .module-visual-filter-row #module_network_legend {
          flex: 0 0 auto;
          margin-left: auto;
        }

        #modules_page .module-visual-shell > hr,
        #modules_page .module-table-shell > hr {
          margin: 0 0 8px 0;
          flex: 0 0 auto;
        }

        #modules_page .module-table-wrap {
          flex: 1 1 auto;
          min-height: 0;
          overflow: auto;
        }

        #modules_page .module-status-wrap {
          flex: 0 0 auto;
        }
        
        #modules_page #module_network_ui {
          display: flex;
          flex: 1;
          min-height: 0;
          width: 100%;
        }

        #modules_page #module_network_ui > .shiny-vis-network,
        #modules_page #module_network_ui > .html-widget,
        #modules_page #module_network_ui .vis-network,
        #modules_page #module_network_ui .vis-network > canvas {
          flex: 1 1 auto;
          min-height: 0;
          width: 100% !important;
          height: 100% !important;
        }
        
        /* Function */
        #function_page .step-main-row {
          align-items: flex-start;
        }

        #function_page .step-main-row > [class*='col-'] {
          display: block;
        }

        #function_page .step-main-row > [class*='col-'] > .card {
          width: 100%;
        }

        #function_page .card > .card-body {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        #function_page[data-sync-panels='ready'] #function_result_card {
          height: var(--function-panel-height);
        }

        #function_page[data-sync-panels='ready'] #function_result_card > .card-body {
          min-height: 0;
          overflow: hidden;
        }

        #function_page[data-sync-panels='ready'] #function_result_card > .card-body > .shiny-html-output {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
        }
        
        #function_page #function_main_ui {
          display: flex;
          flex: 1;
          min-height: 0;
          flex-direction: column;
          width: 100%;
          overflow-y: auto;
          overflow-x: hidden;
        }
      }

      .annotation-table-wrap {
        width: 100%;
        max-width: 100%;
        overflow-x: auto;
      }

      #annotation_page .annotation-table-wrap .dataTables_wrapper {
        width: 100%;
        max-width: 100%;
      }

      #annotation_page .annotation-summary-stack .shiny-plot-output,
      #annotation_page .annotation-summary-stack .shiny-plotly-output,
      #annotation_page .annotation-summary-stack .plotly,
      #annotation_page .annotation-summary-stack .html-widget {
        width: 100% !important;
        max-width: 100% !important;
      }

      #network_page .annotation-table-wrap .dataTables_wrapper {
        width: 100%;
        max-width: 100%;
      }

      #modules_page .annotation-table-wrap .dataTables_wrapper {
        width: 100%;
        max-width: 100%;
      }

      .network-legend-row {
        display: flex;
        flex-direction: row;
        align-items: center;
        gap: 18px;
        margin-top: 0;
        margin-bottom: 0;
        white-space: nowrap;
      }

      .network-legend-item {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        font-size: 14px;
        color: #3a4a5f;
      }

      .network-legend-dot {
        width: 13px;
        height: 13px;
        border-radius: 50%;
        border: 1px solid rgba(0, 0, 0, 0.18);
        display: inline-block;
      }
      
      
      /* Top Navigation Bar */
      .top-navbar {
        background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
        padding: 15px 30px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 0;
      }
      
      .navbar-brand {
        color: white !important;
        font-size: 24px;
        font-weight: 700;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .navbar-brand i {
        font-size: 28px;
      }
      
      /* Navbar brand container */
      .navbar-brand {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      /* Lab logo in navbar */
      .navbar-lab-logo {
        height: 32px;      
        width: auto;
        display: block;
      }

      .nav-pills .nav-link {
        color: rgba(255,255,255,0.8);
        margin: 0 5px;
        border-radius: 20px;
        padding: 8px 20px;
        transition: all 0.3s;
      }
      
      .nav-pills .nav-link:hover {
        background-color: rgba(255,255,255,0.1);
        color: white;
      }
      
      .nav-pills .nav-link.active {
        background-color: rgba(255,255,255,0.2);
        color: white;
      }
      
      /* Home page redesign */
      #home_page {
        background:
          radial-gradient(920px 380px at 10% -10%, rgba(92, 142, 209, 0.16), transparent 65%),
          radial-gradient(840px 360px at 96% 0%, rgba(73, 170, 162, 0.14), transparent 62%),
          #f4f7fb;
        padding: 0 0 54px 0;
        overflow-x: hidden;
      }

      #home_page .home-shell {
        max-width: none;
        margin: 0 auto;
        padding: 0 28px;
      }

      #home_page .hero-section {
        position: relative;
        overflow: hidden;
        border-radius: 0;
        padding: 32px 20px 36px 20px;
        min-height: 56vh;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(128deg, #224a7d 0%, #336ba3 52%, #4ea4a5 100%);
        background-size: 220% 220%;
        animation: heroGradientShift 10s ease-in-out infinite;
        box-shadow: 0 18px 36px rgba(31, 68, 112, 0.28);
        margin: 0 0 24px 0;
      }

      #home_page .hero-section::before {
        content: \"\";
        position: absolute;
        width: 580px;
        height: 580px;
        border-radius: 50%;
        top: -300px;
        right: -180px;
        background: rgba(255, 255, 255, 0.26);
        filter: blur(1px);
        opacity: 0.92;
        animation: heroBlobDriftA 11s ease-in-out infinite alternate;
      }

      #home_page .hero-section::after {
        content: \"\";
        position: absolute;
        width: 500px;
        height: 500px;
        border-radius: 50%;
        bottom: -280px;
        left: -190px;
        background: rgba(255, 255, 255, 0.18);
        filter: blur(0.5px);
        opacity: 0.88;
        animation: heroBlobDriftB 13s ease-in-out infinite alternate;
      }

      @keyframes heroGradientShift {
        0% {
          background-position: 0% 35%;
        }
        50% {
          background-position: 100% 65%;
        }
        100% {
          background-position: 0% 35%;
        }
      }

      @keyframes heroBlobDriftA {
        0% {
          transform: translate3d(0, 0, 0) scale(1);
        }
        100% {
          transform: translate3d(-74px, 46px, 0) scale(1.18);
        }
      }

      @keyframes heroBlobDriftB {
        0% {
          transform: translate3d(0, 0, 0) scale(1);
        }
        100% {
          transform: translate3d(68px, -52px, 0) scale(1.16);
        }
      }

      @media (prefers-reduced-motion: reduce) {
        #home_page .hero-section,
        #home_page .hero-section::before,
        #home_page .hero-section::after {
          animation: none !important;
        }
      }

      #home_page .hero-content {
        position: relative;
        z-index: 2;
        width: min(1100px, 94vw);
        text-align: center;
      }

      #home_page .home-kicker {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        background: rgba(255, 255, 255, 0.18);
        color: #eef6ff;
        border: 1px solid rgba(255, 255, 255, 0.25);
        border-radius: 999px;
        font-size: 13px;
        font-weight: 600;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        padding: 7px 13px;
        margin: 0 auto 20px auto;
      }

      #home_page .hero-title {
        color: #ffffff;
        font-size: clamp(36px, 5vw, 62px);
        line-height: 1.14;
        font-weight: 700;
        margin: 0 0 16px 0;
      }

      #home_page .hero-subtitle {
        color: rgba(240, 248, 255, 0.95);
        font-size: clamp(16px, 1.55vw, 21px);
        line-height: 1.45;
        max-width: none;
        white-space: nowrap;
        margin: 0 auto 24px auto;
      }

      #home_page .hero-buttons {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 12px;
        align-items: center;
      }

      #home_page .btn-hero {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
        padding: 11px 22px;
        min-width: 168px;
        border-radius: 10px;
        font-size: 15px;
        font-weight: 600;
        transition: all 0.2s ease;
      }

      #home_page .btn-hero-primary {
        color: #1f4774;
        background: #ffffff;
        border: 1px solid #ffffff;
      }

      #home_page .btn-hero-primary:hover,
      #home_page .btn-hero-primary:focus {
        color: #1f4774;
        background: #eef5ff;
        border-color: #eef5ff;
        transform: translateY(-1px);
        box-shadow: 0 8px 18px rgba(13, 27, 44, 0.22);
      }

      #home_page .btn-hero-secondary {
        color: #edf6ff;
        background: rgba(10, 28, 50, 0.14);
        border: 1px solid rgba(221, 237, 255, 0.52);
      }

      #home_page .btn-hero-secondary:hover,
      #home_page .btn-hero-secondary:focus {
        color: #ffffff;
        background: rgba(255, 255, 255, 0.18);
        border-color: rgba(255, 255, 255, 0.72);
        transform: translateY(-1px);
      }

      #home_page .hero-side-panel {
        background: rgba(12, 31, 54, 0.24);
        border: 1px solid rgba(255, 255, 255, 0.24);
        border-radius: 14px;
        padding: 16px 18px 14px 18px;
        color: #eef5ff;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
      }

      #home_page .hero-logo {
        width: clamp(220px, 25vw, 320px);
        max-width: 100%;
        height: auto;
        margin: 0 auto 22px auto;
        display: block;
      }

      #home_page .home-section {
        margin-top: 20px;
      }

      #home_page .home-section-head {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 6px;
        margin-bottom: 16px;
        text-align: center;
      }

      #home_page .section-title {
        margin: 0;
        font-size: 29px;
        font-weight: 700;
        color: #223d58;
      }

      #home_page .section-subtitle {
        margin: 0;
        font-size: 14px;
        color: #4d657d;
      }

      #home_page .feature-card {
        background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
        border: 1px solid #d8e2ee;
        border-radius: 14px;
        padding: 22px 18px;
        text-align: center;
        box-shadow: 0 8px 18px rgba(48, 75, 105, 0.12);
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        height: 100%;
      }

      #home_page .feature-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 14px 24px rgba(41, 67, 95, 0.16);
      }

      #home_page .feature-icon {
        width: 60px;
        height: 60px;
        margin: 0 auto 14px auto;
        border-radius: 14px;
        background: linear-gradient(140deg, #2b598e 0%, #3f7cb2 65%, #4fa4a6 100%);
        color: #ffffff;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 26px;
        box-shadow: 0 10px 18px rgba(43, 89, 142, 0.24);
      }

      #home_page .feature-number {
        font-size: 34px;
        font-weight: 700;
        color: #224d7b;
        letter-spacing: 0.01em;
        margin-bottom: 6px;
      }

      #home_page .feature-title {
        font-size: 17px;
        font-weight: 700;
        color: #2a425a;
        margin-bottom: 4px;
      }

      #home_page .feature-desc {
        margin: 0;
        font-size: 13px;
        color: #5b7085;
      }

      #home_page .workflow-section,
      #home_page .citation-section {
        background: transparent;
        padding: 0;
        margin-top: 20px;
      }

      #home_page .workflow-card,
      #home_page .citation-card {
        border-radius: 16px;
        border: 1px solid #d8e2ee;
        background: #ffffff;
        padding: 22px;
        box-shadow: 0 10px 22px rgba(43, 67, 92, 0.12);
      }

      #home_page .citation-card {
        background: linear-gradient(125deg, #1f4065 0%, #2e5e8e 52%, #3f7f8c 100%);
        border: none;
        color: #edf5ff;
        box-shadow: 0 15px 28px rgba(31, 64, 101, 0.28);
      }

      #home_page .citation-text {
        font-size: 16px;
        line-height: 1.7;
      }

      #home_page .citation-card .btn {
        border-radius: 10px;
        padding: 9px 14px;
        font-weight: 600;
      }

      @media (max-width: 1080px) {
        #home_page .hero-section {
          min-height: 50vh;
        }

        #home_page .hero-subtitle {
          white-space: normal;
        }
      }

      @media (max-width: 768px) {
        #home_page {
          padding: 0 0 40px 0;
        }

        #home_page .home-shell {
          padding: 0 14px;
        }

        .content-wrapper {
          padding: 0 14px;
        }

        #home_page .hero-section {
          min-height: 46vh;
          padding: 26px 14px 30px 14px;
        }

        #home_page .hero-title {
          font-size: clamp(34px, 11vw, 46px);
        }

        #home_page .hero-subtitle {
          font-size: 16px;
        }

        #home_page .hero-buttons .btn-hero {
          width: 100%;
        }

        #home_page .home-section-head {
          flex-direction: column;
          align-items: center;
        }

        #home_page .section-title {
          font-size: 24px;
        }
      }

      
      /* Step indicators for other pages */
      .step-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .step-number {
        font-size: 36px;
        font-weight: bold;
        opacity: 0.9;
      }
      
      .step-title {
        font-size: 24px;
        font-weight: 600;
        margin-top: 10px;
      }
      
      .step-description {
        font-size: 14px;
        opacity: 0.9;
        margin-top: 5px;
      }
      
      /* Parameter cards */
      .param-card {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      
      .param-card h4 {
        color: #2c3e50;
        font-size: 16px;
        font-weight: 600;
        margin-bottom: 10px;
      }

      /* Functional annotation result layout */
      .function-result-shell {
        display: flex;
        flex-direction: column;
        gap: 16px;
        width: 100%;
      }

      .function-selector-wrap {
        background: linear-gradient(180deg, #f6f9fc 0%, #edf3fb 100%);
        border: 1px solid #d7e3f2;
        border-radius: 12px;
        padding: 16px 18px 8px 18px;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.05);
      }

      .function-selector-wrap .form-group {
        margin-bottom: 0;
      }

      .function-selector-wrap label {
        font-size: 16px;
        font-weight: 700;
        color: #1f3a5a;
        margin-bottom: 8px;
      }

      .function-selector-wrap .selectize-input {
        min-height: 44px;
        border-radius: 10px;
        border-color: #c8d7ea;
        font-size: 16px;
      }

      /* Module selector (Step 4) */
      #modules_page .module-selector-wrap {
        background: #f3f5f7;
        border: 1px solid #d7dde4;
        border-radius: 12px;
        padding: 14px 16px;
      }

      #modules_page .module-selector-wrap .form-group {
        margin-bottom: 0;
      }

      #modules_page .module-selector-wrap label {
        font-size: 15px;
        font-weight: 700;
        color: #33485e;
        margin-bottom: 8px;
      }

      #modules_page .module-selector-wrap .selectize-control.single .selectize-input {
        min-height: 44px;
        display: flex;
        align-items: center;
        border: 1px solid #cfd7e0;
        border-radius: 8px;
        font-size: 15px;
        color: #2f3f50;
        padding: 9px 12px;
        background: #ffffff;
        box-shadow: inset 0 1px 2px rgba(36, 52, 67, 0.06);
      }

      #modules_page .module-selector-wrap .selectize-input.focus {
        border-color: #9aaec4;
        box-shadow: 0 0 0 0.2rem rgba(138, 156, 176, 0.20);
      }

      #modules_page .module-selector-wrap .selectize-dropdown {
        border-color: #cfd7e0;
        border-radius: 8px;
        box-shadow: 0 10px 20px rgba(55, 70, 84, 0.14);
      }

      #modules_page .module-selector-wrap .selectize-dropdown .option {
        padding: 9px 12px;
        font-size: 14px;
      }

      #modules_page .module-selector-wrap .selectize-dropdown .active {
        background: #eef2f6;
        color: #304356;
      }

      .function-annotation-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        gap: 14px;
      }

      .function-annotation-card {
        background: #fbfcfe;
        border: 1px solid #dbe5f2;
        border-radius: 12px;
        padding: 18px 20px;
        margin-bottom: 0;
      }

      .function-annotation-card h4 {
        display: flex;
        align-items: center;
        gap: 8px;
        color: #23466d;
        font-size: 19px;
        margin-bottom: 10px;
      }

      .function-annotation-title {
        font-size: 23px;
        font-weight: 700;
        color: #16324f;
        line-height: 1.35;
        margin: 0;
      }

      .function-annotation-text {
        font-size: 17px;
        color: #2b2b2b;
        line-height: 1.75;
        margin: 0;
      }

      .function-annotation-confidence {
        display: inline-block;
        font-size: 16px;
        font-weight: 600;
        color: #11495a;
        background-color: #e7f4f7;
        border: 1px solid #c5e3ea;
        border-radius: 999px;
        padding: 6px 14px;
        margin: 0;
      }

      .function-pmid-list {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        max-height: 220px;
        overflow-y: auto;
      }

      .function-pmid-chip {
        display: inline-block;
        padding: 6px 10px;
        border-radius: 999px;
        background: #eef3fa;
        border: 1px solid #d2deed;
        color: #224363;
        font-size: 14px;
        font-weight: 600;
        line-height: 1.2;
      }

      a.function-pmid-chip {
        text-decoration: none;
      }

      a.function-pmid-chip:hover {
        background: #dfeaf8;
        border-color: #bdd2ea;
        color: #153857;
      }

      .function-annotation-note {
        font-size: 15px;
        color: #415366;
        margin-bottom: 12px;
      }

      .function-met-pmid-list {
        display: flex;
        flex-direction: column;
        gap: 12px;
      }

      .function-met-pmid-item {
        border: 1px solid #dbe5f2;
        border-radius: 10px;
        background: #ffffff;
        padding: 12px 14px;
      }

      .function-met-pmid-head {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 8px;
        margin-bottom: 8px;
      }

      .function-met-pmid-name {
        font-size: 15px;
        font-weight: 700;
        color: #1c3957;
      }

      .function-met-pmid-id {
        font-size: 12px;
        color: #45617d;
        background: #f2f5fa;
        border: 1px solid #d6e0ec;
        border-radius: 6px;
        padding: 1px 6px;
      }

      .function-met-pmid-count {
        font-size: 12px;
        color: #39556f;
        background: #e7f0fa;
        border: 1px solid #ccdeef;
        border-radius: 999px;
        padding: 2px 8px;
      }

      .function-met-pmid-empty {
        margin: 0;
      }
      
      /* Card styles */
      .card {
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      
      .card-header {
        background-color: #3966a2;
        color: white;
        font-weight: 600;
        border-radius: 12px 12px 0 0 !important;
      }
      
      /* Result / Output card header */
      .card-header-result {
        background-color: #519d9e;
        color: white;
      }

      /* Buttons */
      .btn-primary {
        background-color: #6191d3;
        border-color: #6191d3;
        border-radius: 6px;
      }
      
      .btn-primary:hover {
        background-color: #5568d3;
        border-color: #5568d3;
      }
      
      .btn-success {
        background-color: #d75725;
        border-color: #d75725;
        border-radius: 6px;
      }
      
      .btn-success:hover {
        background-color: #e86349;
        border-color: #e86349;
      }
      
      .next-button-container {
        display: flex;
        justify-content: center;
        margin-top: 30px;
        margin-bottom: 30px;
      }
      
      .btn-next {
        padding: 10px 24px;
        font-size: 16px;
        font-weight: 600;
      }

      .run-next-row {
        display: flex;
        gap: 10px;
      }

      .run-next-row .btn {
        flex: 1 1 0;
      }

      
      /* Content container */
      .content-wrapper {
        max-width: none;
        width: 100%;
        margin: 0 auto;
        padding: 0 28px;
      }

      /* Results page redesign */
      #results_page {
        background:
          radial-gradient(900px 380px at 10% -8%, rgba(97, 145, 211, 0.14), transparent 65%),
          radial-gradient(820px 340px at 96% 0%, rgba(81, 157, 158, 0.12), transparent 62%),
          #f4f7fb;
        padding: 0 0 26px 0;
      }

      .results-step-box {
        background: linear-gradient(120deg, #274f86 0%, #3f7eb7 56%, #4fa8a8 100%);
        border-radius: 14px;
        padding: 18px 22px;
        margin-bottom: 16px;
        box-shadow: 0 14px 28px rgba(31, 68, 112, 0.24);
      }

      .results-step-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 8px;
      }

      .results-kicker {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        font-size: 13px;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        padding: 6px 12px;
        border-radius: 999px;
        background: rgba(255, 255, 255, 0.2);
      }

      .results-step-number {
        font-size: 14px;
        font-weight: 700;
        padding: 5px 10px;
        border-radius: 999px;
        background: rgba(0, 0, 0, 0.14);
      }

      #results_page .results-overview-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 16px;
      }

      #results_page .results-overview-card {
        background: white;
        border: 1px solid #dbe4f1;
        border-radius: 12px;
        padding: 14px 16px;
        box-shadow: 0 5px 18px rgba(34, 66, 105, 0.08);
      }

      #results_page .results-overview-label {
        font-size: 12px;
        color: #5b6f84;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        margin-bottom: 6px;
      }

      #results_page .results-overview-value {
        font-size: 28px;
        color: #1f4269;
        font-weight: 700;
        line-height: 1.1;
        margin-bottom: 4px;
      }

      #results_page .results-overview-subtext {
        font-size: 13px;
        color: #6b7786;
      }

      #results_page .results-export-card,
      #results_page .results-params-card {
        border: 1px solid #d8e2ef;
        box-shadow: 0 12px 24px rgba(34, 66, 105, 0.09);
      }

      #results_page .results-export-card .card-body,
      #results_page .results-params-card .card-body {
        background: linear-gradient(180deg, #ffffff 0%, #f9fbfe 100%);
      }

      #results_page .results-card-note {
        font-size: 13px;
        color: #5f6e7f;
        margin-bottom: 14px;
      }

      #results_page .checkbox {
        margin-top: 0;
        margin-bottom: 8px;
      }

      #results_page .checkbox label {
        width: 100%;
        display: flex;
        align-items: center;
        gap: 8px;
        background: #f1f6fb;
        border: 1px solid #d7e5f5;
        border-radius: 10px;
        padding: 10px 12px;
        color: #214466;
        font-weight: 500;
      }

      #results_page .checkbox input[type='checkbox'] {
        margin-top: 1px;
      }

      #results_page .form-group > label {
        font-size: 14px;
        font-weight: 700;
        color: #2a3f58;
      }

      #results_page .results-preview-wrap {
        margin-top: 14px;
        margin-bottom: 14px;
        border: 1px solid #d6e3f2;
        border-radius: 10px;
        padding: 12px;
        background: #f7fbff;
      }

      #results_page .results-preview-title {
        display: flex;
        align-items: center;
        gap: 7px;
        font-size: 13px;
        font-weight: 700;
        color: #224a72;
        margin-bottom: 10px;
      }

      #results_page .results-preview-list {
        list-style: none;
        padding: 0;
        margin: 0;
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      #results_page .results-preview-item {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
        background: white;
        border: 1px solid #d8e4f2;
        border-radius: 9px;
        padding: 8px 10px;
      }

      #results_page .results-preview-item-label {
        font-size: 14px;
        color: #1f3f60;
      }

      #results_page .results-preview-status {
        font-size: 12px;
        font-weight: 700;
        border-radius: 999px;
        padding: 4px 10px;
      }

      #results_page .results-preview-status.ready {
        color: #21603b;
        background: #e6f6ec;
        border: 1px solid #c1e8d0;
      }

      #results_page .results-preview-status.pending {
        color: #8a5a15;
        background: #fff3de;
        border: 1px solid #f6dfb7;
      }

      #results_page .results-empty-preview {
        font-size: 13px;
        color: #6b7684;
        margin: 0;
      }

      #results_page .results-download-btn {
        font-size: 17px;
        font-weight: 700;
        border-radius: 10px;
      }

      #results_page .results-network-wrap {
        margin-top: 14px;
        border: 1px solid #d6e3f2;
        border-radius: 10px;
        padding: 12px;
        background: #f7fbff;
      }

      #results_page #analysis_parameters h5 {
        margin: 0 0 8px 0;
        padding: 7px 10px;
        border-left: 4px solid #4f84b7;
        background: #eef4fb;
        border-radius: 6px;
        color: #234a72;
        font-size: 15px;
        font-weight: 700;
      }

      #results_page #analysis_parameters ul {
        margin: 0 0 14px 0;
        padding-left: 20px;
      }

      #results_page #analysis_parameters li {
        margin-bottom: 5px;
        color: #364a5f;
      }

      #results_page #analysis_parameters p {
        margin-bottom: 12px;
        color: #5d6b7a;
      }

      @media (max-width: 991px) {
        #results_page .results-overview-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }

        .results-step-top {
          flex-wrap: wrap;
        }
      }

      @media (max-width: 576px) {
        #results_page .results-overview-grid {
          grid-template-columns: minmax(0, 1fr);
        }

        .results-step-box {
          padding: 14px 16px;
        }
      }

      /* Quickstart guide */
      @keyframes quickstartPulse {
        0% {
          box-shadow: 0 0 0 0 rgba(46, 91, 142, 0.35);
        }
        70% {
          box-shadow: 0 0 0 12px rgba(46, 91, 142, 0);
        }
        100% {
          box-shadow: 0 0 0 0 rgba(46, 91, 142, 0);
        }
      }

      .quickstart-focus {
        position: relative !important;
        z-index: 2265 !important;
        outline: 3px solid #ffd166 !important;
        outline-offset: 3px;
        border-radius: 8px;
        box-shadow: 0 0 0 4px rgba(255, 209, 102, 0.35);
        animation: quickstartPulse 1.4s ease-in-out infinite;
      }

      .quickstart-card {
        background: #ffffff;
        border: 1px solid #cfd9e6;
        border-radius: 12px;
        box-shadow: 0 14px 28px rgba(25, 48, 74, 0.20);
        padding: 12px 14px 10px 14px;
      }

      .quickstart-head {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        margin-bottom: 8px;
      }

      .quickstart-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        font-size: 12px;
        font-weight: 700;
        color: #244a73;
        background: #edf3fb;
        border: 1px solid #cfdff0;
        border-radius: 999px;
        padding: 4px 9px;
      }

      .quickstart-close.btn {
        width: 28px;
        height: 28px;
        min-width: 28px;
        padding: 0;
        border-radius: 50%;
        border: 1px solid #d2dbe7;
        background: #f6f8fb;
        color: #4e6073;
      }

      .quickstart-title {
        margin: 0 0 8px 0;
        font-size: 16px;
        font-weight: 700;
        color: #213d5a;
      }

      .quickstart-body {
        margin: 0;
        color: #3e5165;
        font-size: 13px;
        line-height: 1.45;
      }

      .quickstart-note {
        margin: 8px 0 0 0;
        font-size: 12px;
        color: #5a6a7b;
      }

      .quickstart-actions {
        display: flex;
        gap: 8px;
        margin-top: 10px;
      }

      .quickstart-actions .btn {
        flex: 1;
      }

      .modal-dialog.quickstart-phase-modal {
        max-width: 420px;
        margin: 0;
      }

      .modal-dialog.quickstart-phase-modal .modal-content {
        border-radius: 12px;
        border: 1px solid #d3dce8;
      }

      .modal-dialog.quickstart-phase-modal .modal-header {
        border-bottom: 1px solid #e6ebf2;
        padding: 12px 14px;
      }

      .modal-dialog.quickstart-phase-modal .modal-body {
        padding: 12px 14px 6px 14px;
      }

      .modal-dialog.quickstart-phase-modal .modal-footer {
        border-top: 1px solid #e6ebf2;
        padding: 10px 14px 12px 14px;
      }
      
      /* Compress visNetwork legend width */
      .vis-network .vis-legend {
        max-width: 120px !important;
        padding: 6px 6px !important;
      }
    
      .vis-network .vis-legend-text {
        font-size: 12px !important;
        white-space: nowrap;
      }
    
      .vis-network .vis-legend-color {
        width: 10px !important;
        height: 10px !important;
        margin-right: 6px !important;
      }
      
    ")),

    tags$script(HTML("
      (function() {
        function clearPinnedInfo(exceptEl) {
          document.querySelectorAll('.inline-info-wrap.is-pinned').forEach(function(el) {
            if (!exceptEl || el !== exceptEl) {
              el.classList.remove('is-pinned');
            }
          });
        }

        document.addEventListener('click', function(event) {
          var trigger = event.target.closest('.inline-info-wrap');
          if (trigger) {
            event.stopPropagation();
            var shouldPin = !trigger.classList.contains('is-pinned');
            clearPinnedInfo(trigger);
            if (shouldPin) {
              trigger.classList.add('is-pinned');
            } else {
              trigger.classList.remove('is-pinned');
            }
            return;
          }

          clearPinnedInfo();
        });

        document.addEventListener('keydown', function(event) {
          if (event.key === 'Escape') {
            clearPinnedInfo();
          }
        });
      })();
    ")),

    tags$script(HTML("
    function runCounters() {
      const counters = document.querySelectorAll('.counter');
    
      counters.forEach(counter => {
        const target = +counter.getAttribute('data-value');
        const duration = 2200;
        const stepTime = 30;
        const startTime = performance.now();
    
        counter.innerText = '0';
    
        const timer = setInterval(() => {
          const elapsed = performance.now() - startTime;
          const progress = Math.min(elapsed / duration, 1);
          const eased = 1 - Math.pow(1 - progress, 3);
          const value = Math.floor(eased * target);
    
          counter.innerText = value.toLocaleString();
    
          if (progress === 1) {
            counter.innerText = target.toLocaleString();
            clearInterval(timer);
          }
        }, stepTime);
      });
    }


    document.addEventListener('DOMContentLoaded', runCounters);
	  "))
	    ,
	    tags$script(HTML("
	      (function() {
	        function resizeOnePlotly(el) {
	          if (!el || !window.Plotly) return;
	          var target = el.classList && el.classList.contains('js-plotly-plot') ? el : el.querySelector('.js-plotly-plot');
	          if (!target) return;
	          var parent = target.parentElement || target;
	          var w = parent.getBoundingClientRect ? parent.getBoundingClientRect().width : 0;
	          if (window.Plotly.Plots && window.Plotly.Plots.resize) {
	            try { window.Plotly.Plots.resize(target); } catch (e) {}
	          }
	          if (window.Plotly.relayout && w && w > 0) {
	            try { window.Plotly.relayout(target, {autosize: true, width: Math.floor(w)}); } catch (e) {}
	          }
	        }

	        function resizePlotlyWidgets(selector) {
	          var nodes;
	          if (selector) {
	            nodes = document.querySelectorAll(selector);
	          } else {
	            nodes = document.querySelectorAll('.js-plotly-plot');
	          }
	          nodes.forEach(function(el) { resizeOnePlotly(el); });
	        }

	        window.forcePlotlyResize = function(ids) {
	          var doResize = function() {
	            if (Array.isArray(ids) && ids.length > 0) {
	              ids.forEach(function(id) {
	                var el = document.getElementById(id);
	                if (el) resizeOnePlotly(el);
	              });
	            } else {
	              resizePlotlyWidgets();
	            }
	          };
	          [0, 90, 220, 420, 760].forEach(function(t) {
	            setTimeout(doResize, t);
	          });
	        };

	        document.addEventListener('shown.bs.tab', function() {
	          window.forcePlotlyResize();
	        });

	        window.addEventListener('resize', function() {
	          setTimeout(function() { window.forcePlotlyResize(); }, 20);
	        });
	      })();
	    ")),
	    
	    tags$script(HTML("
	      (function() {
	        var resizeObserver = null;
	        var resizeTimer = null;

	        function applyUploadPanelHeight() {
	          var page = document.getElementById('upload_page');
	          var leftCard = document.getElementById('upload_param_card');
	          if (!page || !leftCard) return;

	          if (window.innerWidth < 992 || page.offsetParent === null) {
	            page.removeAttribute('data-sync-panels');
	            page.style.removeProperty('--upload-panel-height');
	            return;
	          }

	          page.removeAttribute('data-sync-panels');
	          page.style.removeProperty('--upload-panel-height');

	          window.requestAnimationFrame(function() {
	            var height = Math.ceil(leftCard.getBoundingClientRect().height);
	            if (!height || height < 1) return;
	            page.style.setProperty('--upload-panel-height', height + 'px');
	            page.setAttribute('data-sync-panels', 'ready');
	          });
	        }

	        function queueUploadPanelSync() {
	          clearTimeout(resizeTimer);
	          resizeTimer = setTimeout(applyUploadPanelHeight, 30);
	        }

	        function initUploadPanelSync() {
	          var leftCard = document.getElementById('upload_param_card');
	          if (!leftCard || typeof ResizeObserver === 'undefined') return;
	          if (resizeObserver) {
	            resizeObserver.disconnect();
	          }
	          resizeObserver = new ResizeObserver(function() {
	            queueUploadPanelSync();
	          });
	          resizeObserver.observe(leftCard);
	        }

	        document.addEventListener('DOMContentLoaded', function() {
	          initUploadPanelSync();
	          queueUploadPanelSync();
	        });

	        document.addEventListener('shown.bs.tab', function() {
	          queueUploadPanelSync();
	        });

	        window.addEventListener('resize', function() {
	          queueUploadPanelSync();
	        });

	        window.syncUploadPanels = queueUploadPanelSync;
	      })();
	    ")),
	    
	    tags$script(HTML("
	      (function() {
	        var resizeObserver = null;
	        var resizeTimer = null;

	        function applyAnnotationPanelHeight() {
	          var page = document.getElementById('annotation_page');
	          var leftCard = document.getElementById('annotation_param_card');
	          if (!page || !leftCard) return;

	          if (window.innerWidth < 992 || page.offsetParent === null) {
	            page.removeAttribute('data-sync-panels');
	            page.style.removeProperty('--annotation-panel-height');
	            return;
	          }

	          page.removeAttribute('data-sync-panels');
	          page.style.removeProperty('--annotation-panel-height');

	          window.requestAnimationFrame(function() {
	            var height = Math.ceil(leftCard.getBoundingClientRect().height);
	            if (!height || height < 1) return;
	            page.style.setProperty('--annotation-panel-height', height + 'px');
	            page.setAttribute('data-sync-panels', 'ready');
	          });
	        }

	        function queueAnnotationPanelSync() {
	          clearTimeout(resizeTimer);
	          resizeTimer = setTimeout(applyAnnotationPanelHeight, 30);
	        }

	        function initAnnotationPanelSync() {
	          var leftCard = document.getElementById('annotation_param_card');
	          if (!leftCard || typeof ResizeObserver === 'undefined') return;
	          if (resizeObserver) {
	            resizeObserver.disconnect();
	          }
	          resizeObserver = new ResizeObserver(function() {
	            queueAnnotationPanelSync();
	          });
	          resizeObserver.observe(leftCard);
	        }

	        document.addEventListener('DOMContentLoaded', function() {
	          initAnnotationPanelSync();
	          queueAnnotationPanelSync();
	        });

	        document.addEventListener('shown.bs.tab', function() {
	          queueAnnotationPanelSync();
	        });

	        window.addEventListener('resize', function() {
	          queueAnnotationPanelSync();
	        });

	        window.syncAnnotationPanels = queueAnnotationPanelSync;
	      })();
	    ")),
	    
	    tags$script(HTML("
	      (function() {
	        var resizeObserver = null;
	        var resizeTimer = null;

	        function applyNetworkPanelHeight() {
	          var page = document.getElementById('network_page');
	          var leftCard = document.getElementById('network_param_card');
	          if (!page || !leftCard) return;

	          if (window.innerWidth < 992 || page.offsetParent === null) {
	            page.removeAttribute('data-sync-panels');
	            page.style.removeProperty('--network-panel-height');
	            return;
	          }

	          page.removeAttribute('data-sync-panels');
	          page.style.removeProperty('--network-panel-height');

	          window.requestAnimationFrame(function() {
	            var height = Math.ceil(leftCard.getBoundingClientRect().height);
	            if (!height || height < 1) return;
	            page.style.setProperty('--network-panel-height', height + 'px');
	            page.setAttribute('data-sync-panels', 'ready');
	          });
	        }

	        function queueNetworkPanelSync() {
	          clearTimeout(resizeTimer);
	          resizeTimer = setTimeout(applyNetworkPanelHeight, 30);
	        }

	        function initNetworkPanelSync() {
	          var leftCard = document.getElementById('network_param_card');
	          if (!leftCard || typeof ResizeObserver === 'undefined') return;
	          if (resizeObserver) {
	            resizeObserver.disconnect();
	          }
	          resizeObserver = new ResizeObserver(function() {
	            queueNetworkPanelSync();
	          });
	          resizeObserver.observe(leftCard);
	        }

	        document.addEventListener('DOMContentLoaded', function() {
	          initNetworkPanelSync();
	          queueNetworkPanelSync();
	        });

	        document.addEventListener('shown.bs.tab', function() {
	          queueNetworkPanelSync();
	        });

	        window.addEventListener('resize', function() {
	          queueNetworkPanelSync();
	        });

	        window.syncNetworkPanels = queueNetworkPanelSync;
	      })();
	    ")),

	    tags$script(HTML("
	      (function() {
	        var resizeObserver = null;
	        var resizeTimer = null;

	        function applyModulesPanelHeight() {
	          var page = document.getElementById('modules_page');
	          var leftCard = document.getElementById('modules_param_card');
	          if (!page || !leftCard) return;

	          if (window.innerWidth < 992 || page.offsetParent === null) {
	            page.removeAttribute('data-sync-panels');
	            page.style.removeProperty('--modules-panel-height');
	            return;
	          }

	          page.removeAttribute('data-sync-panels');
	          page.style.removeProperty('--modules-panel-height');

	          window.requestAnimationFrame(function() {
	            var height = Math.ceil(leftCard.getBoundingClientRect().height);
	            if (!height || height < 1) return;
	            page.style.setProperty('--modules-panel-height', height + 'px');
	            page.setAttribute('data-sync-panels', 'ready');
	          });
	        }

	        function queueModulesPanelSync() {
	          clearTimeout(resizeTimer);
	          resizeTimer = setTimeout(applyModulesPanelHeight, 30);
	        }

	        function initModulesPanelSync() {
	          var leftCard = document.getElementById('modules_param_card');
	          if (!leftCard || typeof ResizeObserver === 'undefined') return;
	          if (resizeObserver) {
	            resizeObserver.disconnect();
	          }
	          resizeObserver = new ResizeObserver(function() {
	            queueModulesPanelSync();
	          });
	          resizeObserver.observe(leftCard);
	        }

	        document.addEventListener('DOMContentLoaded', function() {
	          initModulesPanelSync();
	          queueModulesPanelSync();
	        });

	        document.addEventListener('shown.bs.tab', function() {
	          queueModulesPanelSync();
	        });

	        window.addEventListener('resize', function() {
	          queueModulesPanelSync();
	        });

	        window.syncModulesPanels = queueModulesPanelSync;
	      })();
	    ")),

	    tags$script(HTML("
	      (function() {
	        var resizeObserver = null;
	        var resizeTimer = null;

	        function applyFunctionPanelHeight() {
	          var page = document.getElementById('function_page');
	          var leftCard = document.getElementById('function_param_card');
	          if (!page || !leftCard) return;

	          if (window.innerWidth < 992 || page.offsetParent === null) {
	            page.removeAttribute('data-sync-panels');
	            page.style.removeProperty('--function-panel-height');
	            return;
	          }

	          page.removeAttribute('data-sync-panels');
	          page.style.removeProperty('--function-panel-height');

	          window.requestAnimationFrame(function() {
	            var height = Math.ceil(leftCard.getBoundingClientRect().height);
	            if (!height || height < 1) return;
	            page.style.setProperty('--function-panel-height', height + 'px');
	            page.setAttribute('data-sync-panels', 'ready');
	          });
	        }

	        function queueFunctionPanelSync() {
	          clearTimeout(resizeTimer);
	          resizeTimer = setTimeout(applyFunctionPanelHeight, 30);
	        }

	        function initFunctionPanelSync() {
	          var leftCard = document.getElementById('function_param_card');
	          if (!leftCard || typeof ResizeObserver === 'undefined') return;
	          if (resizeObserver) {
	            resizeObserver.disconnect();
	          }
	          resizeObserver = new ResizeObserver(function() {
	            queueFunctionPanelSync();
	          });
	          resizeObserver.observe(leftCard);
	        }

	        document.addEventListener('DOMContentLoaded', function() {
	          initFunctionPanelSync();
	          queueFunctionPanelSync();
	        });

	        document.addEventListener('shown.bs.tab', function() {
	          queueFunctionPanelSync();
	        });

	        window.addEventListener('resize', function() {
	          queueFunctionPanelSync();
	        });

	        window.syncFunctionPanels = queueFunctionPanelSync;
	      })();
	    ")),
	    
	    tags$script(HTML("
	      (function() {
	        function fallbackCopyText(text) {
	          return new Promise(function(resolve, reject) {
	            var textarea = document.createElement('textarea');
	            textarea.value = text;
	            textarea.setAttribute('readonly', '');
	            textarea.style.position = 'fixed';
	            textarea.style.top = '-9999px';
	            textarea.style.left = '-9999px';
	            document.body.appendChild(textarea);
	            textarea.focus();
	            textarea.select();

	            try {
	              var copied = document.execCommand('copy');
	              document.body.removeChild(textarea);
	              if (copied) {
	                resolve();
	              } else {
	                reject(new Error('execCommand copy failed'));
	              }
	            } catch (err) {
	              document.body.removeChild(textarea);
	              reject(err);
	            }
	          });
	        }

	        function copyText(text) {
	          if (
	            navigator.clipboard &&
	            typeof navigator.clipboard.writeText === 'function' &&
	            window.isSecureContext
	          ) {
	            return navigator.clipboard.writeText(text).catch(function() {
	              return fallbackCopyText(text);
	            });
	          }
	          return fallbackCopyText(text);
	        }

	        Shiny.addCustomMessageHandler('copy-to-clipboard', function(message) {
	          var text = message && message.text ? String(message.text) : '';
	          var inputId = message && message.inputId ? String(message.inputId) : 'clipboard_copy_result';

	          copyText(text)
	            .then(function() {
	              Shiny.setInputValue(inputId, {
	                ok: true,
	                nonce: Date.now()
	              }, { priority: 'event' });
	            })
	            .catch(function() {
	              Shiny.setInputValue(inputId, {
	                ok: false,
	                nonce: Date.now()
	              }, { priority: 'event' });
	            });
	        });
	      })();
	    ")),
	    
	    tags$script(HTML("
	      window.clearQuickstartTargets = function() {
	        document.querySelectorAll('.quickstart-focus').forEach(function(el) {
	          el.classList.remove('quickstart-focus');
        });
        window.__quickstartTargetId = null;
      };

      window.setQuickstartSingleTarget = function(id) {
        window.clearQuickstartTargets();
        if (!id) return;
        var el = document.getElementById(id);
        if (!el) return;
        try {
          el.scrollIntoView({ behavior: 'smooth', block: 'center', inline: 'nearest' });
        } catch (e) {}
        el.classList.add('quickstart-focus');
        window.__quickstartTargetId = id;
      };

      window.positionQuickstartDialog = function(targetId) {
        var target = document.getElementById(targetId);
        var dialog = document.getElementById('quickstart_phase_dialog');
        if (!target || !dialog) return;

        var modal = dialog.closest('.modal');
        if (modal) {
          modal.style.background = 'transparent';
          modal.style.pointerEvents = 'none';
          modal.style.zIndex = '2275';
        }
        dialog.style.pointerEvents = 'auto';
        dialog.style.position = 'fixed';
        dialog.style.transform = 'none';
        dialog.style.margin = '0';
        dialog.style.zIndex = '2280';

        var tr = target.getBoundingClientRect();
        var vw = window.innerWidth || document.documentElement.clientWidth;
        var vh = window.innerHeight || document.documentElement.clientHeight;
        var dw = dialog.offsetWidth || 380;
        var dh = dialog.offsetHeight || 220;
        var gap = 14;

        var left = tr.right + gap;
        var top = tr.top + (tr.height - dh) / 2;

        if (left + dw + 8 > vw) {
          left = tr.left - dw - gap;
        }
        if (left < 8) {
          left = Math.max(8, vw - dw - 8);
        }
        if (top + dh + 8 > vh) {
          top = vh - dh - 8;
        }
        if (top < 8) {
          top = 8;
        }

        if (
          left < tr.right &&
          left + dw > tr.left &&
          top < tr.bottom &&
          top + dh > tr.top
        ) {
          top = tr.bottom + gap;
          if (top + dh + 8 > vh) {
            top = Math.max(8, tr.top - dh - gap);
          }
        }

        dialog.style.left = Math.round(left) + 'px';
        dialog.style.top = Math.round(top) + 'px';
      };

      window.attachQuickstartDialogTarget = function(targetId) {
        window.__quickstartDialogTargetId = targetId;
        setTimeout(function() {
          window.positionQuickstartDialog(targetId);
        }, 0);
      };

      window.bindQuickstartDialog = function(targetId, tries) {
        var t = (typeof tries === 'number') ? tries : 0;
        var modals = document.querySelectorAll('.modal.show');
        if (!modals || modals.length === 0) {
          if (t < 8) {
            setTimeout(function() { window.bindQuickstartDialog(targetId, t + 1); }, 60);
          }
          return;
        }
        var modal = modals[modals.length - 1];
        var dialog = modal.querySelector('.modal-dialog');
        if (!dialog) {
          if (t < 8) {
            setTimeout(function() { window.bindQuickstartDialog(targetId, t + 1); }, 60);
          }
          return;
        }
        dialog.id = 'quickstart_phase_dialog';
        dialog.classList.add('quickstart-phase-modal');
        window.attachQuickstartDialogTarget(targetId);
      };

      window.addEventListener('resize', function() {
        if (window.__quickstartDialogTargetId) {
          window.positionQuickstartDialog(window.__quickstartDialogTargetId);
        }
      });

      window.addEventListener('scroll', function() {
        if (window.__quickstartDialogTargetId) {
          window.positionQuickstartDialog(window.__quickstartDialogTargetId);
        }
      }, true);
    "))
    
    
    
  ),
  
  # Top Navigation Bar
  tags$div(
    class = "top-navbar",
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; max-width: 1400px; margin: 0 auto;",
      tags$div(
        class = "navbar-brand",
        tags$img(
          src = "shen_lab_logo6.png",
          class = "navbar-lab-logo",
          alt = "Lab Logo"
        ),
        # tags$span("FeatureMAP")
      ),
      tags$ul(
        class = "nav nav-pills",
        style = "margin: 0;",
        tags$li(class = "nav-item", actionLink("nav_home", "Home", class = "nav-link active")),
        tags$li(class = "nav-item", actionLink("nav_upload", "Data Upload", class = "nav-link")),
        tags$li(class = "nav-item", actionLink("nav_annotation", "Feature Annotation", class = "nav-link")),
        tags$li(class = "nav-item", actionLink("nav_network", "PCSF Subnetwork", class = "nav-link")),
        tags$li(class = "nav-item", actionLink("nav_modules", "Module Identification", class = "nav-link")),
        tags$li(class = "nav-item", actionLink("nav_function", "Function Annotation", class = "nav-link")),
        tags$li(class = "nav-item", actionLink("nav_results", "Results", class = "nav-link"))
      )
    )
  ),
  
  # Main Content Area
  tags$div(
    id = "main_content",
    
    # Home Page
    tags$div(
      id = "home_page",

      # Hero Section
      tags$div(
        class = "hero-section",
        tags$div(
          class = "hero-content",
          tags$img(
            src = "pack_shiny.png",
            class = "hero-logo",
            alt = "FeatureMAP Shiny Logo"
          ),
          tags$h1(class = "hero-title", "FeatureMAP (Online Service)"),
          tags$p(
            class = "hero-subtitle",
            "A metabolite feature-based functional module identification and annotation pipeline."
          ),
          tags$div(
            class = "hero-buttons",
            actionButton(
              "get_started",
              "Get Started",
              icon = icon("play"),
              class = "btn btn-hero btn-hero-primary"
            ),
            tags$a(
              href = "https://github.com/tidymass/featuremap_shiny",
              target = "_blank",
              class = "btn btn-hero btn-hero-secondary",
              icon("book"),
              "Documentation"
            )
          )
        )
      ),

      tags$div(
        class = "home-shell",

        # Key Features Section
        tags$div(
          class = "home-section",
          fluidRow(
            column(
              width = 3,
              tags$div(
                class = "feature-card",
                tags$div(class = "feature-icon", icon("database")),
                tags$div(
                  class = "feature-number counter",
                  `data-value` = 9,
                  "0"
                ),
                tags$div(class = "feature-title", "Curated Databases"),
                tags$p(class = "feature-desc", "HMDB, KEGG and supporting resources")
              )
            ),
            column(
              width = 3,
              tags$div(
                class = "feature-card",
                tags$div(class = "feature-icon", icon("cube")),
                tags$div(
                  class = "feature-number counter",
                  `data-value` = 9730,
                  "0"
                ),
                tags$div(class = "feature-title", "Metabolite Nodes"),
                tags$p(class = "feature-desc", "Reference entities in the merged network")
              )
            ),
            column(
              width = 3,
              tags$div(
                class = "feature-card",
                tags$div(class = "feature-icon", icon("network-wired")),
                tags$div(
                  class = "feature-number counter",
                  `data-value` = 31666,
                  "0"
                ),
                tags$div(class = "feature-title", "Reaction Edges"),
                tags$p(class = "feature-desc", "Biochemical links used by module discovery")
              )
            ),
            column(
              width = 3,
              tags$div(
                class = "feature-card",
                tags$div(class = "feature-icon", icon("globe")),
                tags$div(
                  class = "feature-number counter",
                  `data-value` = 1,
                  "0"
                ),
                tags$div(class = "feature-title", "Comprehensive Network"),
                tags$p(class = "feature-desc", "One unified graph across the full pipeline")
              )
            )
          )
        ),

        # Workflow Section
        tags$div(
          class = "workflow-section home-section",
          tags$div(
            class = "home-section-head",
            tags$h2(class = "section-title", "Workflow Overview")
          ),
          tags$div(
            class = "workflow-card",
            plotOutput("workflow_diagram", height = 400)
          )
        ),

        # Citation Section
        tags$div(
          class = "citation-section home-section",
          tags$div(
            class = "home-section-head",
            tags$h2(class = "section-title", "Citation")
          ),
          tags$div(
            class = "citation-card",
            tags$div(
              class = "citation-text",
              tags$p(
                style = "font-size: 18px; margin-bottom: 10px;",
                "If you use FeatureMAP in your research, please cite:"
              ),
              tags$p(
                style = "margin-left: 20px;",
                tags$strong("FeatureMAP: "),
                "A Comprehensive Pipeline for Metabolite Feature-based ",
                "Functional Module Identification and Annotation. ",
                tags$em("Journal Name"),
                ", Year;Volume:Pages."
              )
            ),
            actionButton(
              "copy_citation",
              "Copy Citation",
              icon = icon("copy"),
              class = "btn btn-light"
            )
          )
        )
      )
    ),
    
    # Data Upload Page
    tags$div(
      id = "upload_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box results-step-box",
          tags$div(
            class = "results-step-top",
            tags$div(class = "results-kicker", icon("upload"), "Workflow Step"),
            tags$div(class = "results-step-number", "Step 1")
          ),
          tags$div(class = "step-title", "Data Upload"),
          tags$div(class = "step-description", "Upload your metabolomics feature table and reference databases")
        ),
        # ================= Step 1: Data Upload =================
        fluidRow(
          class = "step-main-row",
          
          # ================= LEFT: Data Upload =================
          column(
            width = 4,
            tags$div(
              id = "upload_param_card",
              class = "card",
              tags$div(class = "card-header", "Data Upload"),
              tags$div(
                class = "card-body",
                tags$div(
                  class = "param-card",
                  tags$h4("Data Tables"),
                  fileInput(
                    "feature_file",
                    "Feature Table (.rda, .csv)",
                    accept = c(".rda", ".csv", ".Rdata")
                  ),
                  tags$small(
                    style = "color:#6c757d;",
                    "Required columns: variable_id, mz, rt, polarity, mean_intensity, condition, and pvalue."
                  ),
                  tags$hr(),
                  tags$br(),
                  fileInput(
                    "ms2_file",
                    "MS2 Annotation (.rda, .csv)",
                    accept = c(".rda", ".csv", ".Rdata")
                  ),
                  tags$small(
                    style = "color:#6c757d;",
                    "This field is optional. Required columns: variable_id, and at least one of HMDB.ID or KEGG.ID."
                  ),
                  tags$hr(),
                  tags$div(
                    style = "display:flex; gap:8px; flex-wrap:wrap;",
                    actionButton(
                      "use_demo_both",
                      "Use Demo Data",
                      icon = icon("flask"),
                      class = "btn btn-outline-secondary"
                    ),
                    downloadButton(
                      "download_demo_data",
                      "Download Demo",
                      class = "btn btn-outline-secondary"
                    )
                  )
                ),
                
                tags$div(
                  class = "param-card",
                  tags$h4(
                    "Reference Source",
                    build_info_icon(
                      paste0(
                        "Choose <code>Default reference</code> to use the built-in MS1 annotation database ",
                        "and metabolic reaction network provided by FeatureMAP.<br><br>",
                        "If you want to supplement or replace these built-in resources, ",
                        "choose <code>Upload reference datasets</code> and provide your own files."
                      )
                    )
                  ),
                  radioButtons(
                    "ref_source",
                    "",
                    choices = c(
                      "Default reference (recommended)" = "default",
                      "Upload reference datasets" = "upload"
                    ),
                    selected = "default"
                  ),
                  conditionalPanel(
                    condition = "input.ref_source == 'upload'",
                    tags$div(
                      tags$div(
                        style = "font-weight:600; margin-bottom:6px;",
                        "MS1 Annotation Database (.rda)",
                        build_info_icon(
                          paste0(
                            "Upload a single object in .rda/.RData format.<br><br>",
                            "It should be a metid-compatible MS1 annotation database whose ",
                            "<code>spectra.info</code> contains at least:<br>",
                            "<code>Lab.ID</code>, <code>Compound.name</code>, <code>Formula</code>, ",
                            "<code>HMDB.ID</code>, <code>KEGG.ID</code>, <code>CAS.ID</code>, ",
                            "<code>mz</code>, <code>RT</code>, and <code>Submitter</code>.<br><br>",
                            "Specific reference-format examples can be downloaded from our GitHub: ",
                            "<a href='https://github.com/tidymass/featuremap_resources' target='_blank'>featuremap_resources</a>"
                          )
                        )
                      ),
                      fileInput(
                        "compound_db_file",
                        NULL,
                        accept = c(".rda", ".RData")
                      )
                    )
                  ),
                  tags$br(),
                  conditionalPanel(
                    condition = "input.ref_source == 'upload'",
                    tags$div(
                      tags$div(
                        style = "font-weight:600; margin-bottom:6px;",
                        "Metabolic Reaction Network (.rda)",
                        build_info_icon(
                          paste0(
                            "Upload a single object in .rda/.RData format.<br><br>",
                            "It should be an <code>igraph</code> metabolic reaction network.<br><br>",
                            "Specific reference-format examples can be downloaded from our GitHub: ",
                            "<a href='https://github.com/tidymass/featuremap_resources' target='_blank'>featuremap_resources</a>"
                          )
                        )
                      ),
                      fileInput(
                        "mrn_file",
                        NULL,
                        accept = c(".rda", ".RData")
                      )
                    )
                  )
                ),
                
                tags$hr(),
                tags$div(
                  class = "run-next-row",
                  actionButton(
                    "load_upload_data",
                    "Load",
                    icon = icon("upload"),
                    class = "btn-success btn-lg"
                  ),
                  actionButton(
                    "next_to_annotation",
                    "Next Step",
                    icon = icon("arrow-right-from-bracket"),
                    class = "btn btn-primary btn-lg",
                    disabled = "disabled"
                  )
                )
              )
            )
          ),
          
          # ================= RIGHT: Data Preview =================
          column(
            width = 8,
            tags$div(
              id = "upload_result_card",
              class = "card",
              tags$div(class = "card-header card-header-result", 
                       icon("table"),
                       "Data Preview"),
              tags$div(
                class = "card-body",
                tags$div(
                  class = "upload-preview-shell",
                  tabsetPanel(
                    id = "upload_preview_tabs",
                    tabPanel(
                      "Feature Table",
                      tags$div(
                        class = "upload-preview-table",
                        DTOutput("feature_preview")
                      )
                    ),
                    tabPanel(
                      "MS2 Table",
                      tags$div(
                        class = "upload-preview-table",
                        DTOutput("ms2_preview")
                      )
                    )
                  )
                ),
                tags$hr(),
                tags$div(
                  class = "upload-status-wrap",
                  uiOutput("upload_status_ui")
                )
              )
            )
          )
        )
      )
    ),
    
    # Annotation Page  
    tags$div(
      id = "annotation_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box results-step-box",
          tags$div(
            class = "results-step-top",
            tags$div(class = "results-kicker", icon("magnifying-glass"), "Workflow Step"),
            tags$div(class = "results-step-number", "Step 2")
          ),
          tags$div(class = "step-title", "Feature Annotation"),
          tags$div(class = "step-description", "Match features to metabolite databases using MS1 or MS2 information")
        ),
        fluidRow(
          class = "step-main-row",
          column(
            width = 4,
            tags$div(
              id = "annotation_param_card",
              class = "card",
              tags$div(class = "card-header", "Parameters"),
              tags$div(
                class = "card-body",
                tags$div(
                  class = "param-card",
                  tags$h4("Matching Tolerance"),
                  sliderInput("ms1_ppm", "MS1 Match (ppm)", 
                              min = 5, max = 30, value = 15, step = 1),
                  sliderInput("rt_tol", "RT Tolerance (second)",
                              min = 1, max = 10, value = 5, step = 1)
                ),
                tags$div(
                  class = "param-card",
                  tags$h4("Advanced Options"),
                  selectInput("column_type", "Column Type",
                              choices = c("Reverse Phase" = "rp", "HILIC" = "hilic")),
                  numericInput("isotope_num", "Isotope Number", 
                               value = 3, min = 1, max = 5),
                  numericInput("workers", "Threads",
                               value = 4, min = 1, max = 16)
                ),
                tags$hr(),
                tags$div(
                  class = "run-next-row",
                  actionButton(
                    "run_annotation", "Run",
                    icon = icon("play-circle"),
                    class = "btn-success btn-lg"
                  ),
                  actionButton(
                    "next_to_network", "Next Step",
                    icon = icon("arrow-right-from-bracket"),
                    class = "btn btn-primary btn-lg",
                    disabled = "disabled"
                  )
                )
              )
            )
          ),
          column(
            width = 8,
            tags$div(
              id = "annotation_result_card",
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("layer-group"),
                "Annotation Results"
              ),
              tags$div(
                class = "card-body",
                # ===== Tabs: content-level =====
                  tabsetPanel(
                  id = "annotation_tabs",
                  type = "tabs",
                  
                  tabPanel(
                    title = tagList(icon("chart-simple"), "Summary"),
                    value = "summary",
                    tags$div(
                      class = "annotation-summary-stack",
                      uiOutput("annotation_mfc_ui"),
                      uiOutput("annotation_feature_ui")
                    )
                  ),
                  
                  tabPanel(
                    title = tagList(icon("table"), "Annotation Table 1"),
                    value = "table1",
                    tags$div(
                      class = "annotation-table-wrap",
                      DTOutput("annotated_table")
                    ),
                    tags$hr(),
                    uiOutput("annotation_status_ui")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Network Extraction Page
    tags$div(
      id = "network_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box results-step-box",
          tags$div(
            class = "results-step-top",
            tags$div(class = "results-kicker", icon("network-wired"), "Workflow Step"),
            tags$div(class = "results-step-number", "Step 3")
          ),
          tags$div(class = "step-title", "Subnetwork Extraction"),
          tags$div(class = "step-description", "Extract optimal subnetwork using Prize-Collecting Steiner Forest algorithm")
        ),
        fluidRow(
          class = "step-main-row",
          
          # ===== LEFT: Parameters =====
          column(
            width = 4,
            tags$div(
              id = "network_param_card",
              class = "card",
              tags$div(class = "card-header", "Parameters"),
              tags$div(
                class = "card-body",
                
                tags$div(
                  class = "param-card",
                  tags$h4("Feature Selection"),
                  selectInput(
                    "pcsf_feature",
                    "Features used as terminals",
                    choices = c(
                      "Significant only" = "sig",
                      "All features" = "all"
                    ),
                    selected = "sig"
                  ),
                  conditionalPanel(
                    condition = "input.pcsf_feature == 'sig'",
                    sliderInput(
                      "pcsf_pcutoff",
                      "Significance cutoff (p-value)",
                      min = 0,
                      max = 1,
                      value = 0.05,
                      step = 0.01
                    )
                  )
                ),
                
                tags$div(
                  class = "param-card",
                  tags$h4("PCSF Parameters"),
                  
                  checkboxInput(
                    "pcsf_grid_tune",
                    "Enable grid tuning",
                    value = FALSE
                  ),
                  
                  conditionalPanel(
                    condition = "input.pcsf_grid_tune",
                    
                    tags$div(
                      style = "margin-left: 5px;",
                      
                      tags$h5("Grid tuning range", style = "margin-top:10px;"),
                      
                      fluidRow(
                        column(
                          4,
                          numericInput(
                            "pcsf_b_min",
                            "b min",
                            value = 30,
                            min = 0,
                            step = 1
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            "pcsf_b_max",
                            "b max",
                            value = 40,
                            min = 0,
                            step = 1
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            "pcsf_b_step",
                            "b step",
                            value = 5,
                            min = 1,
                            step = 1
                          )
                        )
                      ),
                      
                      fluidRow(
                        column(
                          4,
                          numericInput(
                            "pcsf_w_min",
                            "w min",
                            value = 30,
                            min = 0,
                            step = 1
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            "pcsf_w_max",
                            "w max",
                            value = 40,
                            min = 0,
                            step = 1
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            "pcsf_w_step",
                            "w step",
                            value = 5,
                            min = 1,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "!input.pcsf_grid_tune",
                    sliderInput(
                      "pcsf_b",
                      "Node prize weight (b)",
                      min = 20, max = 60, value = 40, step = 1
                    ),
                    sliderInput(
                      "pcsf_w",
                      "Steiner penalty (w)",
                      min = 20, max = 60, value = 30, step = 5
                    )
                  ),
                  
                  numericInput(
                    "pcsf_times",
                    "Iterations (final)",
                    value = 100,
                    min = 1,
                    step = 1
                  ),
                  
                  conditionalPanel(
                    condition = "input.pcsf_grid_tune",
                    numericInput(
                      "pcsf_times_tune",
                      "Iterations (grid tuning)",
                      value = 10,
                      min = 1,
                      step = 1
                    )
                  ),
                  
                  numericInput(
                    "pcsf_workers",
                    "Threads",
                    value = 4,
                    min = 1,
                    max = 16
                  ),
                  
                  checkboxInput(
                    "pcsf_show_network",
                    "Visualize PCSF subnetwork",
                    value = FALSE
                  )
                  
                ),
                
                tags$hr(),
                tags$div(
                  class = "run-next-row",
                  actionButton(
                    "run_pcsf",
                    "Run PCSF",
                    icon = icon("play-circle"),
                    class = "btn-success btn-lg"
                  ),
                  actionButton(
                    "next_to_modules",
                    "Next Step",
                    icon = icon("arrow-right-from-bracket"),
                    class = "btn btn-primary btn-lg",
                    disabled = "disabled"
                  )
                )
              )
            )
          ),
          
          # ===== RIGHT: Status + Network =====
          column(
            width = 8,
            tags$div(
              id = "network_result_card",
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("chart-line"),
                "PCSF Subnetwork"
              ),
              tags$div(
                class = "card-body",

                # ===== Tabs（content-level）=====
                tabsetPanel(
                  id = "pcsf_tabs",
                  type = "tabs",
                  
                  # ---- Tab 1: Subnetwork ----
                  tabPanel(
                    title = tagList(icon("network-wired"), "Subnetwork"),
                    tags$div(
                      class = "pcsf-network-shell",
                      tags$div(
                        class = "pcsf-network-topbar",
                        uiOutput("pcsf_node_selectors"),
                        uiOutput("pcsf_network_legend")
                      ),
                      uiOutput("pcsf_network_container"),
                      uiOutput("pcsf_tuning_summary"),
                      uiOutput("pcsf_selected_annotation_ui")
                    )
                  ),
                  
                  # ---- Tab 2: Annotation Table 2 ----
                  tabPanel(
                    title = tagList(icon("table"), "Annotation Table 2"),
                    tags$div(
                      class = "annotation-table-wrap",
                      DTOutput("annotation_table2")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Module Identification Page
    # ================= Module Identification Page =================
    tags$div(
      id = "modules_page",
      style = "display: none;",
      
      tags$div(
        class = "content-wrapper",
        
        # ---------- Step header ----------
        tags$div(
          class = "step-box results-step-box",
          tags$div(
            class = "results-step-top",
            tags$div(class = "results-kicker", icon("layer-group"), "Workflow Step"),
            tags$div(class = "results-step-number", "Step 4")
          ),
          tags$div(class = "step-title", "Module Identification"),
          tags$div(
            class = "step-description",
            "Identify functional modules using community detection on the PCSF subnetwork"
          )
        ),
        
        # ================= Main panel =================
        fluidRow(
          class = "step-main-row",
          
          # ========== LEFT: Parameters ==========
          column(
            width = 4,
            tags$div(
              id = "modules_param_card",
              class = "card",
              tags$div(class = "card-header", "Parameters"),
              tags$div(
                class = "card-body",
                
                # ---- Algorithm ----
                # ================= Algorithm =================
                tags$div(
                  class = "param-card",
                  tags$h4("Algorithm"),
                  
                  selectInput(
                    "module_method",
                    "Community detection method",
                    choices = c(
                      "Louvain"        = "louvain",
                      "Walktrap"       = "walktrap",
                      "Fast greedy"    = "fast_greedy",
                      "Leading eigen"  = "leading_eigen"
                    ),
                    selected = "louvain"
                  ),
                  
                  # ---- Sub-parameters (conditional) ----
                  conditionalPanel(
                    condition = "input.module_method == 'louvain'",
                    numericInput(
                      "louvain_res",
                      "Resolution",
                      value = 1, min = 0.1, max = 10
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.module_method == 'walktrap'",
                    numericInput(
                      "walktrap_steps",
                      "Number of steps",
                      value = 4, min = 1, max = 10
                    )
                  )
                ),
                
                # ================= Filtering =================
                tags$div(
                  class = "param-card",
                  tags$h4("Filtering"),
                  
                  sliderInput(
                    "node_freq",
                    "Node frequency threshold",
                    min = 0, max = 1, value = 0.05, step = 0.01
                  )
                ),
                
                # ================= Module size =================
                tags$div(
                  class = "param-card",
                  tags$h4("Module size constraint"),
                  
                  numericInput(
                    "min_module_size",
                    "Minimum size",
                    value = 3, min = 2
                  ),
                  
                  numericInput(
                    "max_module_size",
                    "Maximum size",
                    value = 100000, min = 10
                  )
                ),
                
                # ================= Computation =================
                tags$div(
                  class = "param-card",
                  tags$h4("Computation"),
                  
                  numericInput(
                    "n_perm",
                    "Permutations",
                    value = 100, min = 10, max = 1000
                  ),
                  
                  numericInput(
                    "module_seed",
                    "Random seed",
                    value = 123
                  ),
                  
                  numericInput(
                    "module_workers",
                    "Threads",
                    value = 4, min = 1, max = 16
                  )
                ),
                
                tags$hr(),
                tags$div(
                  class = "run-next-row",
                  actionButton(
                    "run_modules",
                    "Run",
                    icon = icon("play-circle"),
                    class = "btn-success btn-lg"
                  ),
                  actionButton(
                    "next_to_function",
                    "Next Step",
                    icon = icon("arrow-right-from-bracket"),
                    class = "btn btn-primary btn-lg",
                    disabled = "disabled"
                  )
                )
              )
            )
          ),
          
          # ================= RIGHT: Outputs =================
          column(
            width = 8,
            tags$div(
              id = "modules_result_card",
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("layer-group"),
                "Module Results"
              ),
              tags$div(
                class = "card-body",

                # ===== Tabs（content-level）=====
                tabsetPanel(
                  id = "module_tabs",
                  type = "tabs",
                  
                  # ---- Tab 1: Visualization ----
                  tabPanel(
                    title = tagList(icon("network-wired"), "Visualization"),
                    tags$div(
                      class = "module-visual-shell",
                      uiOutput("module_selector_ui"),
                      tags$hr(class = "module-selector-divider"),
                      tags$div(
                        class = "module-visual-filter-row",
                        tags$div(
                          id = "module_node_selector_ui",
                          class = "pcsf-selector-item",
                          tags$div(
                            class = "pcsf-selector-input",
                            tags$div(
                              class = "pcsf-selector-field",
                              selectizeInput(
                                "module_metabolite_select",
                                label = NULL,
                                choices = character(0),
                                selected = NULL,
                                multiple = FALSE,
                                width = "100%",
                                options = list(
                                  placeholder = "Metabolite"
                                )
                              )
                            ),
                            build_info_icon("Select a metabolite to highlight its directly connected metabolites in the network.")
                          )
                        ),
                        uiOutput("module_network_legend")
                      ),
                      uiOutput("module_network_ui")
                    )
                  ),
                  
                  # ---- Tab 2: Module Score ----
                  tabPanel(
                    title = tagList(icon("chart-line"), "Module Score"),
                    tags$div(
                      class = "module-table-shell",
                      tags$div(
                        class = "module-table-wrap annotation-table-wrap",
                        DTOutput("module_score")
                      ),
                      tags$hr(),
                      tags$div(
                        class = "module-status-wrap",
                        uiOutput("module_score_status_ui")
                      )
                    )
                  ),
                  
                  # ---- Tab 3: Module Table ----
                  tabPanel(
                    title = tagList(icon("table"), "Module Table"),
                    tags$div(
                      class = "module-table-shell",
                      tags$div(
                        class = "module-table-wrap annotation-table-wrap",
                        DTOutput("module_table")
                      ),
                      tags$hr(),
                      tags$div(
                        class = "module-status-wrap",
                        uiOutput("module_table_status_ui")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Function Annotation Page
    tags$div(
      id = "function_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box results-step-box",
          tags$div(
            class = "results-step-top",
            tags$div(class = "results-kicker", icon("robot"), "Workflow Step"),
            tags$div(class = "results-step-number", "Step 5")
          ),
          tags$div(class = "step-title", "AI-Powered Function Annotation"),
          tags$div(class = "step-description", "Automatically annotate module functions using large language models")
        ),
        
        fluidRow(
          class = "step-main-row",
          
          # ===== LEFT: Parameters =====
          column(
            width = 4,
            tags$div(
              id = "function_param_card",
              class = "card",
              tags$div(class = "card-header", "Parameters"),
              tags$div(
                class = "card-body",
                
                # ================= Module selection =================
                tags$div(
                  class = "param-card",
                  tags$h4("Module Selection"),
                  
                  selectInput(
                    "func_p_type",
                    "Significance metric",
                    choices = c("p-value" = "p", "FDR" = "fdr"),
                    selected = "fdr"
                  ),
                  
                  numericInput(
                    "func_p_cutoff",
                    "Significance cutoff",
                    value = 0.05,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  
                  helpText("Only significant modules will be annotated.")
                ),
                
                # ================= Study context =================
                tags$div(
                  class = "param-card",
                  tags$h4("Study Context"),
                  
                  textInput(
                    "func_species",
                    "Species (optional)",
                    placeholder = "e.g. human"
                  ),
                  
                  textInput(
                    "func_biospecimen",
                    "Biospecimen (optional)",
                    placeholder = "e.g. plasma, serum, liver"
                  ),
                  
                  textInput(
                    "func_phenotype",
                    "Phenotype (optional)",
                    placeholder = "e.g. diabetes, cancer"
                  )
                ),
                
                # ================= Metabolite class database =================
                tags$div(
                  class = "param-card",
                  tags$h4("Reference Database"),
                  
                  radioButtons(
                    "func_class_source",
                    "Class annotation",
                    choices = c(
                      "Use default database" = "default",
                      "Upload new dataset" = "upload"
                    ),
                    selected = "default"
                  ),
                  
                  conditionalPanel(
                    condition = "input.func_class_source == 'upload'",
                    fileInput(
                      "func_class_file",
                      "Choose reference dataset (.rda / .csv)",
                      accept = c(".rda", ".RData", ".csv")
                    )
                  )
                ),
                
                # ================= PubMed =================
                tags$div(
                  class = "param-card",
                  tags$h4("Literature-based Annotation"),
                  
                  checkboxInput(
                    "func_use_pubmed",
                    "Use PubMed literature",
                    value = FALSE
                  ),
                  
                  conditionalPanel(
                    condition = "input.func_use_pubmed",

                    numericInput(
                      "func_pubmed_years",
                      "Years to search",
                      value = 5, min = 1, max = 20
                    ),
                    
                    numericInput(
                      "func_top_paper",
                      "Top papers retained",
                      value = 3, min = 1, max = 10
                    ),
                    
                    sliderInput(
                      "func_min_similarity",
                      "Minimum semantic similarity",
                      min = 0.3, max = 1,
                      value = 0.6, step = 0.05
                    ),
                    
                  passwordInput(
                    "func_ncbi_key",
                    "NCBI API Key"
                  )
                  )
                ),
                
                # ================= LLM =================
                tags$div(
                  class = "param-card",
                  tags$h4("Large Language Model"),
                  
                  selectInput(
                    "func_provider",
                    "Provider",
                    choices = c(
                      "OpenAI" = "openai",
                      "Google Gemini" = "gemini",
                      "SiliconFlow" = "siliconflow"
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.func_use_pubmed",
                    tags$div(
                      style = "font-weight: 600; margin-bottom: 6px; display: flex; align-items: center; gap: 8px;",
                      "Embedding model",
                      build_info_icon(
                        paste0(
                          "Use the exact embedding-model ID supported by the selected provider.<br><br>",
                          "You can look up available model names here:<br>",
                          "<a href='https://platform.openai.com/docs/models' target='_blank'>OpenAI models</a><br>",
                          "<a href='https://ai.google.dev/gemini-api/docs/models' target='_blank'>Gemini API models</a><br>",
                          "<a href='https://siliconflow.cn/models' target='_blank'>SiliconFlow models</a>"
                        )
                      )
                    ),
                    textInput(
                      "func_embed_model",
                      NULL,
                      value = "",
                      placeholder = "e.g. text-embedding-3-small"
                    )
                  ),
                  
                  tags$div(
                    style = "font-weight: 600; margin-bottom: 6px; display: flex; align-items: center; gap: 8px;",
                    "LLM model",
                    build_info_icon(
                      paste0(
                        "Use the exact chat/generation-model ID supported by the selected provider.<br><br>",
                        "You can look up available model names here:<br>",
                        "<a href='https://platform.openai.com/docs/models' target='_blank'>OpenAI models</a><br>",
                        "<a href='https://ai.google.dev/gemini-api/docs/models' target='_blank'>Gemini API models</a><br>",
                        "<a href='https://siliconflow.cn/models' target='_blank'>SiliconFlow models</a>"
                      )
                    )
                  ),
                  textInput(
                    "func_llm_model",
                    NULL,
                    placeholder = "e.g. gpt-4.1-mini"
                  ),
                  
                  
                  passwordInput(
                    "func_llm_key",
                    "LLM API Key"
                  )
                ),
                
                tags$hr(),
                tags$div(
                  class = "run-next-row",
                  actionButton(
                    "run_function_annotation",
                    "Run",
                    icon = icon("play-circle"),
                    class = "btn-success btn-lg"
                  ),
                  actionButton(
                    "next_to_results",
                    "Go to Results",
                    icon = icon("download"),
                    class = "btn btn-primary btn-lg"
                  )
                )
              )
            )
          ),
          
          
          # ===== RIGHT: Outputs =====
          column(
            width = 8,
            
            tags$div(
              id = "function_result_card",
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("robot"),
                "Module Functional Annotation"
              ),
              tags$div(
                class = "card-body",
                uiOutput("function_main_ui")
              )
              
            )
          )
        ),
        tags$br()
      )
    ),
    
    # Results Export Page
    tags$div(
      id = "results_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box results-step-box",
          tags$div(
            class = "results-step-top",
            tags$div(class = "results-kicker", icon("check-circle"), "Pipeline complete"),
            tags$div(class = "results-step-number", "Step 6")
          ),
          tags$div(class = "step-title", "Download & Report Center"),
          tags$div(
            class = "step-description",
            "Review generated outputs, pick the tables you need, and download a packaged archive."
          )
        ),
        uiOutput("results_overview"),
        fluidRow(
          class = "step-main-row",
          
          # ===== LEFT: Export options =====
          column(
            width = 4,
            tags$div(
              id = "results_export_card",
              class = "card results-export-card",
              tags$div(class = "card-header", icon("file-export"), " Export Options"),
              tags$div(
                class = "card-body",
                tags$p(
                  class = "results-card-note",
                  "Select table format and network format. A single ZIP package will include selected tables and selected networks."
                ),
                checkboxGroupInput(
                  "export_items",
                  "Select Tables to Export",
                  choices = c(
                    "Annotation table 1"   = "anno1",
                    "Annotation table 2"   = "anno2",
                    "Module score"         = "module_score",
                    "Module table"         = "module_table",
                    "Module function"      = "module_function"
                  )
                ),
                selectInput("export_format", "Table Format",
                            choices = c("Excel (.xlsx)" = "xlsx",
                                        "CSV (multiple files)" = "csv",
                                        "RData" = "rdata")),
                tags$div(
                  class = "results-network-wrap",
                  tags$div(class = "results-preview-title", icon("sitemap"), "Network Export"),
                  checkboxGroupInput(
                    "network_export_items",
                    "Select Networks to Export",
                    choices = c(
                      "PCSF subnetwork" = "pcsf",
                      "Module network" = "module"
                    )
                  ),
                  uiOutput("results_module_export_selector"),
                  selectInput(
                    "network_export_format",
                    "Network Format",
                    choices = c(
                      "Interactive HTML (.html)" = "html",
                      "GraphML (.graphml)" = "graphml"
                    ),
                    selected = "html"
                  )
                ),
                tags$div(
                  class = "results-preview-wrap",
                  tags$div(class = "results-preview-title", icon("list-ul"), "Selected Content"),
                  uiOutput("export_preview")
                )
                ,
                tags$div(
                  class = "run-next-row",
                  downloadButton("download_results", "Download Results",
                                 class = "btn-success btn-lg results-download-btn"),
                  actionButton(
                    "back_home_reset",
                    "Back to Home",
                    icon = icon("house"),
                    class = "btn btn-outline-danger btn-lg"
                  )
                )
              )
            )
          ),
          # ===== RIGHT: Analysis Parameters =====
          column(
            width = 8,
            tags$div(
              id = "results_params_card",
              class = "card results-params-card",
              tags$div(
                class = "card-header card-header-result",
                icon("sliders-h"),
                " Analysis Parameters"
              ),
              tags$div(
                class = "card-body",
                uiOutput("analysis_parameters")
              )
            )
          )
        ),
      )
    )
  )
)


# Server Logic==================================================================
server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(
    ### Step 1
    feature_data = NULL,
    ms2_data = NULL,
    staged_feature_data = NULL,
    staged_ms2_data = NULL,
    upload_note = NULL,
    upload_note_type = "info",
    current_page = "home",
    ### Step 2
    annotation_object   = NULL,   
    annotated_table     = NULL,   # anno_feature_table1
    annotation_summary  = NULL,   # redundancy summary
    annotation_status   = "idle",
    ### Step 3
    mrn = NULL,
    annotation_table2 = NULL,
    pcsf_status = "idle",
    pcsf_selected_node = NULL,
    ### Step 4
    module_res    = NULL,
    module_selected_node = NULL,
    module_status = "idle",
    ### Step 5
    function_result = NULL,
    function_status = "idle",
    function_pubmed_used = FALSE,
    results_export_tables_touched = FALSE,
    results_export_networks_touched = FALSE,
    results_export_tables_updating = FALSE,
    results_export_networks_updating = FALSE,
    ### Quickstart
    quickstart_active = FALSE,
    quickstart_prompt_shown = FALSE,
    quickstart_step = NA_integer_,
    quickstart_phase = 1L,
    quickstart_completed_pages = character(0)
  )

  observe({
    upload_ready <- !is.null(rv$feature_data) &&
      !is.null(rv$ref_database) &&
      !is.null(rv$mrn)
    if (upload_ready) {
      shinyjs::enable("next_to_annotation")
    } else {
      shinyjs::disable("next_to_annotation")
    }

    if (rv$annotation_status == "finished") {
      shinyjs::enable("next_to_network")
    } else {
      shinyjs::disable("next_to_network")
    }

    if (rv$pcsf_status == "finished") {
      shinyjs::enable("next_to_modules")
    } else {
      shinyjs::disable("next_to_modules")
    }

    if (rv$module_status == "finished") {
      shinyjs::enable("next_to_function")
    } else {
      shinyjs::disable("next_to_function")
    }

    shinyjs::enable("next_to_results")
  })
  
  # Navigation system
  pages <- c("home", "upload", "annotation", "network", "modules", "function", "results", "docs")
  
  # Function to switch pages
  switch_page <- function(page) {
    lapply(pages, function(p) {
      shinyjs::hide(paste0(p, "_page"))
    })
    shinyjs::show(paste0(page, "_page"))
    
    # Update nav link active states
    lapply(pages, function(p) {
      shinyjs::removeClass(paste0("nav_", p), "active")
    })
    shinyjs::addClass(paste0("nav_", page), "active")

	    shinyjs::runjs(
	      "setTimeout(function() {
	        window.dispatchEvent(new Event('resize'));
	        if (window.syncUploadPanels) {
	          window.syncUploadPanels();
	        }
	        if (window.syncAnnotationPanels) {
	          window.syncAnnotationPanels();
	        }
	        if (window.syncNetworkPanels) {
	          window.syncNetworkPanels();
	        }
	        if (window.syncModulesPanels) {
	          window.syncModulesPanels();
	        }
	        if (window.syncFunctionPanels) {
	          window.syncFunctionPanels();
	        }
	        if (window.forcePlotlyResize) {
	          window.forcePlotlyResize();
	        } else if (window.Plotly && window.Plotly.Plots && window.Plotly.Plots.resize) {
	          document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
	            try { window.Plotly.Plots.resize(el); } catch (e) {}
          });
        }
      }, 140);"
    )
    
    rv$current_page <- page
  }

  reset_pipeline_state <- function() {
    rv$feature_data <- NULL
    rv$ms2_data <- NULL
    rv$staged_feature_data <- NULL
    rv$staged_ms2_data <- NULL
    rv$mrn <- NULL
    rv$ref_database <- NULL
    rv$ref_database_type <- NULL

    rv$upload_note <- NULL
    rv$upload_note_type <- "info"

    rv$annotation_object <- NULL
    rv$annotated_table <- NULL
    rv$annotation_summary <- NULL
    rv$annotation_status <- "idle"

    rv$annotation_table2 <- NULL
    rv$pcsf_status <- "idle"
    rv$pcsf_selected_node <- NULL

    rv$module_res <- NULL
    rv$module_status <- "idle"

    rv$function_result <- NULL
    rv$function_status <- "idle"
    rv$function_pubmed_used <- FALSE

    rv$quickstart_active <- FALSE
    rv$quickstart_step <- NA_integer_
    rv$quickstart_phase <- 1L
    rv$quickstart_completed_pages <- character(0)
    rv$quickstart_prompt_shown <- FALSE

    rv$results_export_tables_touched <- FALSE
    rv$results_export_networks_touched <- FALSE
    rv$results_export_tables_updating <- FALSE
    rv$results_export_networks_updating <- FALSE
    updateCheckboxGroupInput(session, "export_items", selected = character(0))
    updateCheckboxGroupInput(session, "network_export_items", selected = character(0))
  }

  quickstart_step_from_page <- function(page) {
    switch(
      page,
      upload = 1L,
      annotation = 2L,
      network = 3L,
      modules = 4L,
      `function` = 5L,
      results = 6L,
      NA_integer_
    )
  }

  quickstart_plan_for_page <- function(page) {
    switch(
      page,
      upload = list(
        list(target = "upload_param_card", title = "Step 1 Guide: Parameters", text = "You can choose upload parameters and reference settings in this panel."),
        list(target = "load_upload_data", title = "Step 1 Guide: Load", text = "Click this button to load your selected data and references."),
        list(target = "upload_result_card", title = "Step 1 Guide: Result", text = "Check load message and data preview in this result panel.")
      ),
      annotation = list(
        list(target = "annotation_param_card", title = "Step 2 Guide: Parameters", text = "Set feature annotation parameters in this panel."),
        list(target = "run_annotation", title = "Step 2 Guide: Run", text = "Click this button to start feature annotation."),
        list(target = "annotation_result_card", title = "Step 2 Guide: Result", text = "Check summary and annotation table in this result panel.")
      ),
      network = list(
        list(target = "network_param_card", title = "Step 3 Guide: Parameters", text = "Set subnetwork extraction parameters in this panel."),
        list(target = "run_pcsf", title = "Step 3 Guide: Run", text = "Click this button to run PCSF subnetwork extraction."),
        list(target = "network_result_card", title = "Step 3 Guide: Result", text = "Check subnetwork output and annotation table 2 in this result panel.")
      ),
      modules = list(
        list(target = "modules_param_card", title = "Step 4 Guide: Parameters", text = "Set module identification parameters in this panel."),
        list(target = "run_modules", title = "Step 4 Guide: Run", text = "Click this button to run module identification."),
        list(target = "modules_result_card", title = "Step 4 Guide: Result", text = "Check module network, module score, and module table in this result panel.")
      ),
      `function` = list(
        list(target = "function_param_card", title = "Step 5 Guide: Parameters", text = "Set functional annotation options and provide required API key(s) in this panel."),
        list(target = "run_function_annotation", title = "Step 5 Guide: Run", text = "Click this button to run AI-powered functional annotation."),
        list(target = "function_result_card", title = "Step 5 Guide: Result", text = "Check functional annotation outputs in this result panel.")
      ),
      results = list(
        list(target = "results_export_card", title = "Step 6 Guide: Export Options", text = "Choose which result tables to export and select output format in this panel."),
        list(target = "download_results", title = "Step 6 Guide: Download", text = "Click this button to download selected outputs as a zip package."),
        list(target = "results_params_card", title = "Step 6 Guide: Review", text = "Review analysis parameters and exported content in this panel.")
      ),
      NULL
    )
  }

  quickstart_phase_info <- function(page, phase_idx) {
    plan <- quickstart_plan_for_page(page)
    if (is.null(plan) || length(plan) == 0) return(NULL)
    idx <- phase_idx
    if (is.null(idx) || is.na(idx) || idx < 1) idx <- 1L
    if (idx > length(plan)) idx <- length(plan)
    list(
      phase = plan[[idx]],
      idx = idx,
      total = length(plan)
    )
  }
  
  show_process_modal <- function(title, text) {
    showModal(
      modalDialog(
        easyClose = FALSE,
        footer = NULL,
        tags$div(
          class = "fm-process-modal",
          tags$div(
            id = "fm_modal_icon",
            class = "fm-modal-icon running",
            icon("spinner", class = "fa-spin")
          ),
          tags$div(id = "fm_modal_title", class = "fm-modal-title", title),
          tags$div(id = "fm_modal_text", class = "fm-modal-text", text)
        )
      )
    )
    shinyjs::runjs(
      "(function() {
        setTimeout(function() {
          var modal = document.querySelector('.modal.show');
          if (modal) {
            modal.classList.add('fm-process-modal-wrap');
            var dialog = modal.querySelector('.modal-dialog');
            var content = modal.querySelector('.modal-content');
            if (dialog) {
              dialog.classList.add('modal-dialog-centered');
              dialog.classList.add('fm-modal-dialog');
            }
            if (content) {
              content.classList.add('fm-modal-content');
            }
          }
        }, 0);
      })();"
    )
  }
  
  update_process_modal <- function(status = c("success", "error"), title, text) {
    status <- match.arg(status)
    icon_html <- if (status == "success") {
      as.character(icon("check"))
    } else {
      as.character(icon("times"))
    }
    
    shinyjs::runjs(
      glue::glue(
        "(function() {{
          var icon = document.getElementById('fm_modal_icon');
          var titleEl = document.getElementById('fm_modal_title');
          var textEl = document.getElementById('fm_modal_text');
          if (icon) {{
            icon.className = 'fm-modal-icon {status}';
            icon.innerHTML = {jsonlite::toJSON(icon_html, auto_unbox = TRUE)};
          }}
          if (titleEl) {{
            titleEl.textContent = {jsonlite::toJSON(title, auto_unbox = TRUE)};
          }}
          if (textEl) {{
            textEl.textContent = {jsonlite::toJSON(text, auto_unbox = TRUE)};
          }}
        }})();"
      )
    )
  }
  
  close_process_modal <- function(delay_ms = 900) {
    shinyjs::delay(delay_ms, removeModal())
  }
  
  # Navigation observers
  observeEvent(input$nav_home, { switch_page("home") })
  observeEvent(input$nav_upload, { switch_page("upload") })
  observeEvent(input$nav_annotation, { switch_page("annotation") })
  observeEvent(input$nav_network, { switch_page("network") })
  observeEvent(input$nav_modules, { switch_page("modules") })
  observeEvent(input$nav_function, { switch_page("function") })
  observeEvent(input$nav_results, { switch_page("results") })
  observeEvent(input$nav_docs, { switch_page("docs") })

  observeEvent(input$back_home_reset, {
    showModal(
      modalDialog(
        title = tagList(icon("triangle-exclamation"), "Confirm Reset"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_back_home_reset", "Yes, Clear and Back to Home", class = "btn btn-danger")
        ),
        tags$p("This will clear current analysis results and return to Home."),
        tags$p(style = "margin-bottom:0; font-weight:600;", "Do you want to continue?")
      )
    )
  })

  observeEvent(input$confirm_back_home_reset, {
    removeModal()
    reset_pipeline_state()
    shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
    switch_page("home")
    showNotification("Analysis results cleared. Returned to Home.", type = "message")
  })

  show_quickstart_phase_modal <- function() {
    if (!isTRUE(rv$quickstart_active)) {
      removeModal()
      shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
      return(invisible(NULL))
    }
    if (rv$current_page %in% rv$quickstart_completed_pages) {
      removeModal()
      shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
      return(invisible(NULL))
    }

    info <- quickstart_phase_info(rv$current_page, rv$quickstart_phase)
    if (is.null(info)) {
      removeModal()
      shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
      return(invisible(NULL))
    }

    phase <- info$phase

    removeModal()
    showModal(
      modalDialog(
        title = paste0("Guide (", info$idx, "/", info$total, ")"),
        easyClose = FALSE,
        footer = tagList(
          actionButton("quickstart_exit_tip", "Exit Guide", class = "btn btn-outline-secondary"),
          actionButton("quickstart_next_tip", if (info$idx >= info$total) "Finish" else "Next", class = "btn btn-primary")
        ),
        tags$div(class = "quickstart-title", phase$title),
        tags$p(class = "quickstart-body", phase$text)
      )
    )
    shinyjs::delay(
      160,
      shinyjs::runjs(
        glue::glue(
          "window.setQuickstartSingleTarget && window.setQuickstartSingleTarget({jsonlite::toJSON(phase$target, auto_unbox = TRUE)}); window.bindQuickstartDialog && window.bindQuickstartDialog({jsonlite::toJSON(phase$target, auto_unbox = TRUE)});"
        )
      )
    )
    invisible(NULL)
  }

  observeEvent(
    list(rv$quickstart_active, rv$quickstart_phase, rv$current_page),
    {
      show_quickstart_phase_modal()
    },
    ignoreInit = TRUE
  )

  observe({
    if (!identical(rv$current_page, "upload")) return()
    if (isTRUE(rv$quickstart_prompt_shown)) return()

    rv$quickstart_prompt_shown <- TRUE
    showModal(
      modalDialog(
        title = tagList(icon("rocket"), "Quickstart"),
        easyClose = TRUE,
        footer = tagList(
          actionButton("quickstart_skip", "Not now", class = "btn btn-outline-secondary"),
          actionButton("quickstart_start", "Start Quick Guide", class = "btn btn-primary")
        ),
        tags$p("Start a quick guide and follow hints from Step 1 to Step 6.")
      )
    )
  })

  observeEvent(input$quickstart_start, {
    removeModal()
    rv$quickstart_active <- TRUE
    rv$quickstart_step <- 1L
    rv$quickstart_phase <- 1L
    rv$quickstart_completed_pages <- character(0)
    switch_page("upload")
  })

  observeEvent(input$quickstart_skip, {
    removeModal()
    rv$quickstart_active <- FALSE
    rv$quickstart_step <- NA_integer_
    rv$quickstart_phase <- 1L
    rv$quickstart_completed_pages <- character(0)
    shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
  })

  observeEvent(rv$current_page, {
    if (isTRUE(rv$quickstart_active)) {
      rv$quickstart_step <- quickstart_step_from_page(rv$current_page)
      has_plan <- !is.null(quickstart_phase_info(rv$current_page, 1L))
      already_done <- rv$current_page %in% rv$quickstart_completed_pages
      if (isTRUE(has_plan) && !isTRUE(already_done)) {
        rv$quickstart_phase <- 1L
      } else {
        removeModal()
        shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
      }
    } else {
      shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
    }
  })

  observeEvent(input$quickstart_exit_tip, {
    rv$quickstart_active <- FALSE
    rv$quickstart_step <- NA_integer_
    rv$quickstart_phase <- 1L
    rv$quickstart_completed_pages <- character(0)
    removeModal()
    shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
  })

  observeEvent(input$quickstart_next_tip, {
    if (!isTRUE(rv$quickstart_active)) return()
    info <- quickstart_phase_info(rv$current_page, rv$quickstart_phase)
    if (is.null(info)) return()
    if (info$idx < info$total) {
      rv$quickstart_phase <- info$idx + 1L
    } else {
      rv$quickstart_completed_pages <- unique(c(rv$quickstart_completed_pages, rv$current_page))
      rv$quickstart_phase <- 1L
      removeModal()
      shinyjs::runjs("window.clearQuickstartTargets && window.clearQuickstartTargets(); window.__quickstartDialogTargetId = null;")
    }
  })
  
  # Get started button
  observeEvent(input$get_started, {
    switch_page("upload")
  })
  
  # Copy citation button
  observeEvent(input$copy_citation, {
    session$sendCustomMessage(
      "copy-to-clipboard",
      list(
        text = citation_text,
        inputId = "copy_citation_result"
      )
    )
  })

  observeEvent(input$copy_citation_result, {
    if (isTRUE(input$copy_citation_result$ok)) {
      showNotification("Citation copied to clipboard!", type = "message")
    } else {
      showNotification(
        "Clipboard copy failed. Please copy the citation manually.",
        type = "error"
      )
    }
  })
  
  # Workflow diagram (delete later)
  output$workflow_diagram <- renderPlot({
    par(mar = c(1, 1, 1, 1), bg = "transparent")
    plot(1:5, rep(1, 5), type = "n", xlim = c(0, 6), ylim = c(0, 2),
         xlab = "", ylab = "", axes = FALSE)
    
    steps <- c("Upload", "Annotate", "Extract", "Identify", "Interpret")
    colors <- c("#667eea", "#9b59b6", "#e74c3c", "#f39c12", "#27ae60")
    
    for (i in 1:5) {
      rect(i - 0.4, 0.5, i + 0.4, 1.5, col = colors[i], border = NA)
      text(i, 1, steps[i], col = "white", font = 2, cex = 1.3)
      if (i < 5) {
        arrows(i + 0.45, 1, i + 0.55, 1, length = 0.15, lwd = 4, col = "#34495e")
      }
    }
  })
  
  
  ## Step 1: Data upload================================================
  observeEvent(
    list(input$ref_source, input$compound_db_file, input$mrn_file),
    ignoreInit = TRUE,
    {
      if (!is.null(rv$ref_database) || !is.null(rv$mrn)) {
        rv$ref_database <- NULL
        rv$mrn <- NULL
        rv$ref_database_type <- NULL
        rv$upload_note <- "Reference settings changed. Please click Load again."
        rv$upload_note_type <- "warning"
      }
    }
  )
  
  check_feature_columns <- function(feature_tbl) {
    req_cols <- c(
      "variable_id",
      "mz",
      "rt",
      "polarity",
      "mean_intensity",
      "condition",
      "pvalue"
    )
    missing_cols <- setdiff(req_cols, names(feature_tbl))
    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "Feature table is missing required columns: ",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
    invisible(TRUE)
  }

  observeEvent(input$load_upload_data, {
    missing <- character(0)
    use_staged_demo <- is.null(input$feature_file) && !is.null(rv$staged_feature_data)

    if (is.null(input$feature_file) && !use_staged_demo) {
      missing <- c(missing, "Feature Table")
    }
    if (input$ref_source == "upload") {
      if (is.null(input$mrn_file)) {
        missing <- c(missing, "Metabolic Reaction Network")
      }
      if (is.null(input$compound_db_file)) {
        missing <- c(missing, "MS1 Annotation Database")
      }
    }
    if (length(missing) > 0) {
      rv$upload_note <- paste0("Load failed. Missing: ", paste(missing, collapse = ", "))
      rv$upload_note_type <- "error"
      return()
    }
    req(input$ref_source)

    loaded <- tryCatch({
      if (use_staged_demo) {
        feature_tbl <- rv$staged_feature_data
        ms2_tbl <- rv$staged_ms2_data
        data_source <- "demo"
      } else {
        # feature table is mandatory
        feature_ext <- tolower(tools::file_ext(input$feature_file$name))
        feature_tbl <- if (feature_ext == "csv") {
          read.csv(input$feature_file$datapath, check.names = FALSE)
        } else {
          e <- new.env()
          load(input$feature_file$datapath, envir = e)
          e[[ls(e)[1]]]
        }

        # ms2 table is optional
        ms2_tbl <- NULL
        if (!is.null(input$ms2_file)) {
          ms2_ext <- tolower(tools::file_ext(input$ms2_file$name))
          ms2_tbl <- if (ms2_ext == "csv") {
            read.csv(input$ms2_file$datapath, check.names = FALSE)
          } else {
            e <- new.env()
            load(input$ms2_file$datapath, envir = e)
            e[[ls(e)[1]]]
          }
        }
        data_source <- "uploaded"
      }
      check_feature_columns(feature_tbl)

      # references
      if (input$ref_source == "default") {
        e_mrn <- new.env()
        load("www/metabolic_reaction_network.rda", envir = e_mrn)
        mrn_obj <- e_mrn[[ls(e_mrn)[1]]]

        e_ref <- new.env()
        load("www/featuremap_compound.rda", envir = e_ref)
        ref_label <- "Built-in merged database"
        ref_db_obj <- e_ref[[ls(e_ref)[1]]]
      } else {
        req(input$mrn_file)
        e_mrn <- new.env()
        load(input$mrn_file$datapath, envir = e_mrn)
        mrn_obj <- e_mrn[[ls(e_mrn)[1]]]

        e_ref <- new.env()
        req(input$compound_db_file)
        load(input$compound_db_file$datapath, envir = e_ref)
        ref_label <- "Uploaded MS1 annotation database"
        ref_db_obj <- e_ref[[ls(e_ref)[1]]]
      }

      list(
        feature_tbl = feature_tbl,
        ms2_tbl = ms2_tbl,
        mrn_obj = mrn_obj,
        ref_db_obj = ref_db_obj,
        ref_label = ref_label,
        data_source = data_source
      )
    }, error = function(e) e)

    if (inherits(loaded, "error")) {
      rv$upload_note <- paste("Load failed:", loaded$message)
      rv$upload_note_type <- "error"
      return()
    }

    rv$feature_data <- loaded$feature_tbl
    rv$ms2_data <- loaded$ms2_tbl
    rv$mrn <- loaded$mrn_obj
    rv$ref_database <- loaded$ref_db_obj
    rv$ref_database_type <- loaded$ref_label
    rv$staged_feature_data <- NULL
    rv$staged_ms2_data <- NULL

    # reset downstream results after uploading a new input set
    rv$annotation_object <- NULL
    rv$annotated_table <- NULL
    rv$annotation_summary <- NULL
    rv$annotation_status <- "idle"
    rv$annotation_table2 <- NULL
    rv$pcsf_status <- "idle"
    rv$pcsf_selected_node <- NULL
    rv$module_res <- NULL
    rv$module_status <- "idle"
    rv$function_result <- NULL
    rv$function_status <- "idle"
    rv$function_pubmed_used <- FALSE

    ms2_note <- if (is.null(loaded$ms2_tbl)) {
      "MS2 not provided"
    } else {
      paste0("MS2 ", nrow(loaded$ms2_tbl), "x", ncol(loaded$ms2_tbl))
    }
    rv$upload_note <- "Data and references have been loaded successfully!"
    rv$upload_note_type <- "success"
  })

  observeEvent(input$use_demo_both, {
    staged <- tryCatch({
      e_feat <- new.env()
      load("www/demo_feature_table.rda", envir = e_feat)
      feature_tbl <- e_feat[[ls(e_feat)[1]]]
      check_feature_columns(feature_tbl)

      e_ms2 <- new.env()
      load("www/demo_ms2.rda", envir = e_ms2)
      ms2_tbl <- e_ms2[[ls(e_ms2)[1]]] %>%
        dplyr::ungroup() %>%
        dplyr::select(variable_id, dplyr::any_of(c("HMDB.ID", "KEGG.ID", "SS", "Level")))

      list(
        feature_tbl = feature_tbl,
        ms2_tbl = ms2_tbl
      )
    }, error = function(e) e)

    if (inherits(staged, "error")) {
      rv$upload_note <- paste("Demo prepare failed:", staged$message)
      rv$upload_note_type <- "error"
      showNotification(
        paste("Demo prepare failed:", staged$message),
        type = "error",
        duration = 4
      )
      return()
    }

    rv$staged_feature_data <- staged$feature_tbl
    rv$staged_ms2_data <- staged$ms2_tbl

    rv$upload_note <- "Demo data is prepared. Click Load to continue."
    rv$upload_note_type <- "info"
    showNotification(
      "Demo data is prepared. Click Load to continue.",
      type = "message",
      duration = 3
    )
  })
  
  
  
  output$upload_status_ui <- renderUI({
    if (is.null(rv$feature_data)) {
      return(NULL)
    }

    ms2_supported_n <- 0L
    if (!is.null(rv$ms2_data) && is.data.frame(rv$ms2_data) && "variable_id" %in% colnames(rv$ms2_data)) {
      ms2_tbl <- rv$ms2_data

      has_hmdb <- "HMDB.ID" %in% colnames(ms2_tbl)
      has_kegg <- "KEGG.ID" %in% colnames(ms2_tbl)

      if (has_hmdb || has_kegg) {
        ms2_tbl <- ms2_tbl %>%
          dplyr::mutate(
            HMDB.ID = if (has_hmdb) HMDB.ID else NA_character_,
            KEGG.ID = if (has_kegg) KEGG.ID else NA_character_
          ) %>%
          dplyr::filter(
            (!is.na(HMDB.ID) & HMDB.ID != "") |
              (!is.na(KEGG.ID) & KEGG.ID != "")
          ) %>%
          dplyr::distinct(variable_id)

        ms2_supported_n <- nrow(ms2_tbl)
      }
    }

    tags$div(
      class = "param-card",
      tags$h4(icon("circle-check"), "Upload Status"),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Features loaded: ", format(nrow(rv$feature_data), big.mark = ","))
      ),
      tags$p(
        style = "margin-bottom:0;",
        paste0("MS2-supported features: ", format(ms2_supported_n, big.mark = ","))
      )
    )
  })

  output$feature_preview <- renderDT({
    if (is.null(rv$feature_data)) {
      return(render_table_message("No data loaded yet"))
    }
    datatable(
      rv$feature_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  output$download_demo_data <- downloadHandler(
    filename = function() {
      paste0("featuremap_demo_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      resolve_demo_file <- function(filename) {
        candidate_paths <- c(
          system.file(file.path("app", "www", filename), package = "featuremapshiny"),
          system.file(file.path("www", filename), package = "featuremapshiny"),
          file.path("www", filename),
          file.path("inst", "app", "www", filename)
        )
        candidate_paths <- unique(candidate_paths[nzchar(candidate_paths)])
        candidate_paths <- candidate_paths[file.exists(candidate_paths)]

        if (length(candidate_paths) == 0) {
          stop("Missing demo file: ", filename)
        }

        candidate_paths[1]
      }

      demo_files <- c(
        resolve_demo_file("demo_feature_table.rda"),
        resolve_demo_file("demo_ms2.rda")
      )

      tmpdir <- tempfile("featuremap_demo_")
      dir.create(tmpdir)
      file.copy(demo_files, file.path(tmpdir, basename(demo_files)), overwrite = TRUE)

      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(tmpdir)
      utils::zip(zipfile = file, files = basename(demo_files))
    }
  )

  output$ms2_preview <- renderDT({
    if (is.null(rv$ms2_data)) {
      return(render_table_message("No data loaded yet"))
    }
    datatable(
      rv$ms2_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  render_table_message <- function(msg) {
    datatable(
      data.frame(Message = msg),
      options = list(dom = "t"),
      rownames = FALSE
    )
  }
  
  observeEvent(input$next_to_annotation, {
    req(!is.null(rv$feature_data), !is.null(rv$ref_database), !is.null(rv$mrn))
    switch_page("annotation")
  })
  
  ## Step 2 Feature annotation====================================
  observeEvent(input$run_annotation, {
    
    req(rv$feature_data, rv$ref_database)
    
    show_process_modal(
      title = "Step 2 Running",
      text = "Feature annotation is in progress. Please wait."
    )
    
    rv$annotation_status <- "running"
    shinyjs::disable("run_annotation")
    shinyjs::delay(100, {
      res <- tryCatch({
        annotate_feature(
          feature_table = rv$feature_data,
          ms2_table = rv$ms2_data,
          metabolite_database = rv$ref_database,
          column = input$column_type,
          ms1_match_ppm = input$ms1_ppm,
          mfc_rt_tol = input$rt_tol,
          isotope_number = input$isotope_num,
          workers = input$workers
        )
      }, error = function(e) e)
      
      if (inherits(res, "error")) {
        rv$annotation_status <- "error"
        showNotification(res$message, type = "error")
        update_process_modal("error", "Step 2 Failed", res$message)
        close_process_modal(1800)
        shinyjs::enable("run_annotation")
      } else {
        rv$annotation_object  <- res
        rv$annotated_table    <- res$anno_feature_table1
        rv$annotation_summary <- res$anno_redundancy
        rv$annotation_status  <- "finished"
        shinyjs::runjs(
          "window.forcePlotlyResize && window.forcePlotlyResize(['annotation_mfc_plot','annotation_feature_plot']);"
        )
        update_process_modal(
          "success",
          "Step 2 Completed",
          "Feature annotation finished successfully."
        )
        close_process_modal(900)
        
        shinyjs::enable("run_annotation")
      }
      
      shinyjs::enable("run_annotation")
    })
  })

  observeEvent(input$next_to_network, {
    req(rv$annotation_status == "finished")
    switch_page("network")
  })

  observeEvent(input$annotation_tabs, {
    if (!identical(input$annotation_tabs, "summary")) return()
    shinyjs::runjs(
      "window.forcePlotlyResize && window.forcePlotlyResize(['annotation_mfc_plot','annotation_feature_plot']);"
    )
  }, ignoreInit = TRUE)

  observeEvent(rv$current_page, {
    if (!identical(rv$current_page, "annotation")) return()
    shinyjs::runjs(
      "window.forcePlotlyResize && window.forcePlotlyResize(['annotation_mfc_plot','annotation_feature_plot']);"
    )
  }, ignoreInit = TRUE)
  
  output$annotation_mfc_ui <- renderUI({
    if (rv$annotation_status == "running") {
      tags$div(
        class = "empty-panel empty-panel-compact empty-panel-fill",
        icon("spinner", class = "fa-spin"),
        "Preparing metabolite-level summary ..."
      )
    } else if (rv$annotation_status == "finished") {
      plotlyOutput("annotation_mfc_plot", width = "100%", height = 280)
    } else {
      tags$div(
        class = "empty-panel empty-panel-compact empty-panel-fill",
        icon("chart-bar"),
        "No metabolite-level summary yet."
      )
    }
  })
  
  output$annotation_feature_ui <- renderUI({
    if (rv$annotation_status == "running") {
      tags$div(
        class = "empty-panel empty-panel-compact empty-panel-fill",
        icon("spinner", class = "fa-spin"),
        "Preparing feature-level summary ..."
      )
    } else if (rv$annotation_status == "finished") {
      plotlyOutput("annotation_feature_plot", width = "100%", height = 280)
    } else {
      tags$div(
        class = "empty-panel empty-panel-compact empty-panel-fill",
        icon("chart-line"),
        "No feature-level summary yet."
      )
    }
  })

  pick_first_col <- function(df, candidates) {
    nms <- names(df)
    idx <- match(tolower(candidates), tolower(nms))
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) return(NULL)
    nms[idx[1]]
  }

  output$annotation_status_ui <- renderUI({
    if (rv$annotation_status == "running") {
      return(
        tags$div(
          class = "param-card",
          tags$h4(icon("spinner", class = "fa-spin"), "Annotation Status"),
          tags$p(style = "margin-bottom:0;", "Annotation is running...")
        )
      )
    }

    if (is.null(rv$annotated_table) || nrow(rv$annotated_table) == 0) {
      return(
        tags$div(
          class = "param-card",
          tags$h4(icon("circle-info"), "Annotation Status"),
          tags$p(style = "margin-bottom:0;", "No annotation results yet.")
        )
      )
    }

    tbl <- rv$annotated_table
    n_entries <- nrow(tbl)
    n_total_features <- if (!is.null(rv$feature_data)) nrow(rv$feature_data) else NA_integer_

    feature_col <- pick_first_col(
      tbl,
      c("variable_id", "feature_id", "feature", "feature_name", "id")
    )

    n_annotated_features <- if (!is.null(feature_col)) {
      length(unique(stats::na.omit(tbl[[feature_col]])))
    } else {
      n_entries
    }

    coverage_txt <- if (!is.na(n_total_features) && n_total_features > 0) {
      sprintf("%.1f%%", 100 * n_annotated_features / n_total_features)
    } else {
      "NA"
    }

    avg_txt <- if (n_annotated_features > 0) {
      sprintf("%.2f", n_entries / n_annotated_features)
    } else {
      "NA"
    }

    tags$div(
      class = "param-card",
      tags$h4(icon("circle-check"), "Annotation Status"),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Annotated features: ", n_annotated_features,
               if (!is.na(n_total_features)) paste0(" / ", n_total_features, " (", coverage_txt, ")") else "")
      ),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Feature-metabolite entries: ", n_entries)
      ),
      tags$p(
        style = "margin-bottom:0;",
        paste0("Avg metabolites per annotated feature: ", avg_txt)
      )
    )
  })
  
  build_annotation_summary_plot <- function(value_col, y_title, digits, colors) {
    df <- rv$annotation_summary %>%
      tibble::rownames_to_column("stage") %>%
      dplyr::mutate(
        stage = factor(tolower(as.character(stage)), levels = c("before", "after")),
        stage_label = dplyr::if_else(stage == "before", "Before", "After"),
        value = as.numeric(.data[[value_col]]),
        fill_color = dplyr::if_else(stage == "before", colors[1], colors[2]),
        label = format(round(value, digits), nsmall = digits, trim = TRUE)
      ) %>%
      dplyr::arrange(stage)
    
    req(nrow(df) > 0)
    
    before_val <- df$value[df$stage == "before"]
    after_val <- df$value[df$stage == "after"]
    delta_text <- ""
    if (length(before_val) == 1 && length(after_val) == 1 &&
        is.finite(before_val) && before_val != 0) {
      delta_pct <- (after_val - before_val) / before_val * 100
      sign <- ifelse(delta_pct >= 0, "+", "-")
      delta_text <- sprintf("After vs before: %s%.1f%%", sign, abs(delta_pct))
    }
    
    y_max <- max(df$value, na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    
    p <- plot_ly(
      data = df,
      x = ~stage_label,
      y = ~value,
      type = "bar",
      width = 0.55,
      marker = list(
        color = ~fill_color,
        line = list(color = "rgba(255,255,255,0.95)", width = 1.4)
      ),
      text = ~label,
      textposition = "outside",
      cliponaxis = FALSE,
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        y_title, ": %{y:.", digits, "f}<extra></extra>"
      ),
      showlegend = FALSE
    ) %>%
      add_trace(
        data = df,
        x = ~stage_label,
        y = ~value,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 11,
          color = ~fill_color,
          line = list(color = "white", width = 2)
        ),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      layout(
        margin = list(l = 62, r = 6, t = 38, b = 52),
        plot_bgcolor = "rgba(249,252,255,1)",
        paper_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.45,
        xaxis = list(
          title = "",
          tickfont = list(size = 12, color = "#33485f"),
          showgrid = FALSE
        ),
        yaxis = list(
          title = y_title,
          range = c(0, y_max * 1.30),
          gridcolor = "rgba(76,104,132,0.12)",
          zerolinecolor = "rgba(76,104,132,0.20)",
          tickfont = list(size = 12, color = "#33485f"),
          titlefont = list(size = 13, color = "#2f4257")
        ),
        shapes = if (length(before_val) == 1 && is.finite(before_val)) list(
          list(
            type = "line",
            x0 = -0.4, x1 = 1.4,
            y0 = before_val, y1 = before_val,
            line = list(color = "rgba(79,106,133,0.35)", width = 1, dash = "dot")
          )
        ) else list()
      )
    
    if (nzchar(delta_text)) {
      p <- p %>%
        layout(
          annotations = list(
            list(
              x = 1,
              y = 1.08,
              xref = "paper",
              yref = "paper",
              text = delta_text,
              showarrow = FALSE,
              font = list(size = 12, color = "#35587a"),
              bgcolor = "rgba(230,240,249,0.85)",
              bordercolor = "rgba(83,117,151,0.35)",
              borderwidth = 1,
              borderpad = 4
            )
          )
        )
    }
    
    p %>% config(displayModeBar = FALSE, responsive = TRUE)
  }
  
  output$annotation_mfc_plot <- renderPlotly({
    req(rv$annotation_summary)
    build_annotation_summary_plot(
      value_col = "mfc_per_metabolite",
      y_title = "MFC per metabolite",
      digits = 3,
      colors = c("#bfd6ec", "#2f79af")
    )
  })
  
  output$annotation_feature_plot <- renderPlotly({
    req(rv$annotation_summary)
    build_annotation_summary_plot(
      value_col = "metabolite_per_feature",
      y_title = "Metabolites per feature",
      digits = 2,
      colors = c("#f0c9bb", "#cf6445")
    )
  })
  
  output$annotated_table <- renderDT({
    if (is.null(rv$annotated_table) || nrow(rv$annotated_table) == 0) {
      return(render_table_message("No annotation table 1 results yet"))
    }
    datatable(
      rv$annotated_table,
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  ## Step 3: Extract Subnetwork ======================================
  observeEvent(input$run_pcsf, {
    
    req(rv$annotation_object, rv$mrn)
    
    rv$pcsf_status <- "running"
    rv$pcsf_selected_node <- NULL
    shinyjs::disable("run_pcsf")
    
    b_grid <- if (isTRUE(input$pcsf_grid_tune)) {
      seq(
        from = input$pcsf_b_min,
        to   = input$pcsf_b_max,
        by   = input$pcsf_b_step
      )
    } else {
      NULL
    }
    
    w_grid <- if (isTRUE(input$pcsf_grid_tune)) {
      seq(
        from = input$pcsf_w_min,
        to   = input$pcsf_w_max,
        by   = input$pcsf_w_step
      )
    } else {
      NULL
    }
    
    if (isTRUE(input$pcsf_grid_tune)) {
      if (input$pcsf_b_min > input$pcsf_b_max) {
        showNotification("b min must be ≤ b max", type = "error")
        shinyjs::enable("run_pcsf")
        rv$pcsf_status <- "idle"
        return()
      }
      if (input$pcsf_w_min > input$pcsf_w_max) {
        showNotification("w min must be ≤ w max", type = "error")
        shinyjs::enable("run_pcsf")
        rv$pcsf_status <- "idle"
        return()
      }
    }
    
    show_process_modal(
      title = "Step 3 Running",
      text = "PCSF subnetwork extraction is in progress. Please wait."
    )
    
    shinyjs::delay(100, {
      
      res <- tryCatch({
        extract_subnetwork(
          res = rv$annotation_object,
          feature = input$pcsf_feature,
          pcutoff = input$pcsf_pcutoff,
          mrn = rv$mrn,
          workers = input$pcsf_workers,
          
          b = input$pcsf_b,
          w = input$pcsf_w,
          
          grid_tune = input$pcsf_grid_tune,
          b_grid = b_grid,
          w_grid = w_grid,
          times_tune = if (isTRUE(input$pcsf_grid_tune)) input$pcsf_times_tune else 10,
          
          mu = 1e-4,
          r = 0.30,
          times = input$pcsf_times
        )
      }, error = function(e) e)
      
      if (inherits(res, "error")) {
        rv$pcsf_status <- "idle"
        showNotification(res$message, type = "error")
        update_process_modal("error", "Step 3 Failed", res$message)
        close_process_modal(1800)
      } else {
        rv$annotation_object <- res
        rv$annotation_table2 <- res$anno_feature_table2
        rv$pcsf_status <- "finished"
        rv$pcsf_selected_node <- NULL
        update_process_modal(
          "success",
          "Step 3 Completed",
          "Subnetwork extraction finished successfully."
        )
        close_process_modal(900)
      }
      
      shinyjs::enable("run_pcsf")
    })
  })
  
  output$pcsf_tuning_summary <- renderUI({
    
    req(rv$pcsf_status == "finished")
    req(!is.null(rv$annotation_object$tuning_best))
    
    tb <- rv$annotation_object$tuning_best
    
    tags$div(
      class = "param-card",
      style = "margin-top:20px;",
      tags$p(
        tags$strong("Best parameters for grid tuning: "),
        glue::glue("b = {tb$b}, w = {tb$w}")
      ),
    )
  })
  
  output$pcsf_network_container <- renderUI({
    stage_style <- "display:flex; flex:1 1 auto; min-height:0; height:100%; width:100%;"
    fill_style <- "display:flex; flex:1 1 auto; min-height:100%; height:100%; width:100%;"

    if (rv$pcsf_status == "running") {
      tags$div(
        class = "pcsf-network-stage",
        style = stage_style,
        tags$div(
          class = "empty-panel empty-panel-fill",
          style = fill_style,
          icon("spinner", class = "fa-spin"),
          "Running PCSF subnetwork extraction..."
        )
      )
    } else if (rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network)) {
      tags$div(
        class = "pcsf-network-stage",
        style = stage_style,
        visNetworkOutput("pcsf_network_plot", height = 560)
      )
    } else if (rv$pcsf_status == "finished") {
      tags$div(
        class = "pcsf-network-stage",
        style = stage_style,
        tags$div(
          class = "empty-panel empty-panel-fill",
          style = fill_style,
          icon("eye-slash"),
          "Network visualization is disabled. Enable it in parameters."
        )
      )
    } else {
      tags$div(
        class = "pcsf-network-stage",
        style = stage_style,
        tags$div(
          class = "empty-panel empty-panel-fill",
          style = fill_style,
          icon("network-wired"),
          "Subnetwork has not been generated yet."
        )
      )
    }
  })

  output$pcsf_node_selectors <- renderUI({
    if (!(rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network))) {
      return(NULL)
    }

    tags$div(
      class = "pcsf-selector-row",
      tags$div(
        class = "pcsf-selector-item",
          tags$div(
            class = "pcsf-selector-input",
            tags$div(
              class = "pcsf-selector-field",
            selectizeInput(
              "pcsf_feature_select",
              label = NULL,
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = list(
                placeholder = "Feature"
                )
              )
            ),
          build_info_icon("Select a feature to highlight its directly connected metabolites in the network and update the linked rows shown below.")
        )
      ),
      tags$div(
        class = "pcsf-selector-item",
          tags$div(
            class = "pcsf-selector-input",
            tags$div(
              class = "pcsf-selector-field",
            selectizeInput(
              "pcsf_metabolite_select",
              label = NULL,
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = list(
                placeholder = "Metabolite"
                )
              )
            ),
          build_info_icon("Select a metabolite to highlight its directly connected features in the network and update the linked rows shown below.")
        )
      )
    )
  })

  output$pcsf_network_legend <- renderUI({
    if (!(rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network))) {
      return(NULL)
    }
    tags$div(
      class = "network-legend-row",
      tags$span(
        class = "network-legend-item",
        tags$span(class = "network-legend-dot", style = "background:#fc8d58;"),
        "Feature"
      ),
      tags$span(
        class = "network-legend-item",
        tags$span(class = "network-legend-dot", style = "background:#4474b3;"),
        "Metabolite"
      )
    )
  })

  observeEvent(list(rv$pcsf_status, input$pcsf_show_network), {
    if (rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network)) {
      rv$pcsf_selected_node <- NULL
    }
  }, ignoreInit = FALSE)

  observeEvent(list(rv$pcsf_status, input$pcsf_show_network, rv$annotation_object$pcsf_subnet), {
    req(rv$pcsf_status == "finished")
    req(isTRUE(input$pcsf_show_network))
    req(rv$annotation_object$pcsf_subnet)

    lookup <- pcsf_node_lookup()
    feature_lookup <- lookup[lookup$group == "Feature", , drop = FALSE]
    metabolite_lookup <- lookup[lookup$group == "Metabolite", , drop = FALSE]

    updateSelectizeInput(
      session,
      "pcsf_feature_select",
      choices = stats::setNames(feature_lookup$id, feature_lookup$label),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "pcsf_metabolite_select",
      choices = stats::setNames(metabolite_lookup$id, metabolite_lookup$label),
      selected = character(0),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  pcsf_node_lookup <- reactive({
    if (!(rv$pcsf_status == "finished") || is.null(rv$annotation_object$pcsf_subnet)) {
      return(data.frame(
        id = character(0),
        type = character(0),
        group = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      ))
    }

    g <- rv$annotation_object$pcsf_subnet
    node_id <- as.character(igraph::V(g)$name)
    node_type <- as.character(igraph::V(g)$type)
    if (length(node_type) != length(node_id) || all(is.na(node_type))) {
      node_type <- rep("Node", length(node_id))
    }
    node_group <- ifelse(node_type == "Terminal", "Feature", "Metabolite")

    node_compound_name <- igraph::V(g)$compound_name
    if (is.null(node_compound_name) || length(node_compound_name) != length(node_id)) {
      node_compound_name <- rep(NA_character_, length(node_id))
    }
    node_compound_name <- as.character(node_compound_name)

    node_label <- ifelse(
      node_group == "Metabolite" & !is.na(node_compound_name) & nzchar(node_compound_name),
      node_compound_name,
      node_id
    )

    data.frame(
      id = node_id,
      type = node_type,
      group = node_group,
      label = node_label,
      stringsAsFactors = FALSE
    )
  })

  pcsf_selected_node_info <- reactive({
    lookup <- pcsf_node_lookup()
    selected_id <- rv$pcsf_selected_node

    if (is.null(selected_id) || !nzchar(selected_id)) {
      return(NULL)
    }

    hit <- match(selected_id, lookup$id)
    if (is.na(hit)) {
      return(NULL)
    }

    as.list(lookup[hit, , drop = FALSE])
  })

  pcsf_selected_annotation_data <- reactive({
    tbl <- rv$annotation_table2
    selected <- pcsf_selected_node_info()

    if (is.null(tbl) || nrow(tbl) == 0 || is.null(selected)) {
      return(NULL)
    }

    feature_col <- pick_first_col(
      tbl,
      c("variable_id", "feature_id", "feature", "feature_name", "id")
    )
    metabolite_col <- pick_first_col(
      tbl,
      c("INCHI_ID", "inchi_id", "InChI_ID", "compound_id", "Compound_ID")
    )

    if (identical(selected$group, "Feature") && !is.null(feature_col)) {
      return(tbl[tbl[[feature_col]] == selected$id, , drop = FALSE])
    }

    if (identical(selected$group, "Metabolite") && !is.null(metabolite_col)) {
      return(tbl[tbl[[metabolite_col]] == selected$id, , drop = FALSE])
    }

    tbl[0, , drop = FALSE]
  })

  pcsf_network_base_data <- reactive({
    req(rv$pcsf_status == "finished")
    req(isTRUE(input$pcsf_show_network))
    req(rv$annotation_object$pcsf_subnet)

    g <- rv$annotation_object$pcsf_subnet
    lookup <- pcsf_node_lookup()

    node_id <- lookup$id
    node_prize <- igraph::V(g)$prize
    if (is.null(node_prize) || all(is.na(node_prize))) {
      node_prize <- rep(1, length(node_id))
    }
    node_prize <- as.numeric(node_prize)

    feature_fill <- "#fc8d58"
    metabolite_fill <- "#4474b3"

    nodes <- data.frame(
      id = node_id,
      label = lookup$label,
      size = scales::rescale(node_prize, to = c(10, 40)),
      color.background = ifelse(lookup$group == "Feature", feature_fill, metabolite_fill),
      color.border = ifelse(lookup$group == "Feature", feature_fill, metabolite_fill),
      color.highlight.background = ifelse(lookup$group == "Feature", feature_fill, metabolite_fill),
      color.highlight.border = ifelse(lookup$group == "Feature", feature_fill, metabolite_fill),
      color.hover.background = ifelse(lookup$group == "Feature", feature_fill, metabolite_fill),
      color.hover.border = ifelse(lookup$group == "Feature", feature_fill, metabolite_fill),
      font.color = "#475467",
      font.face = "Arial",
      font.size = 14,
      borderWidth = 0,
      stringsAsFactors = FALSE
    )

    edges_df <- igraph::as_data_frame(g, what = "edges")
    edges_df$from <- as.character(edges_df$from)
    edges_df$to <- as.character(edges_df$to)

    edge_freq <- igraph::E(g)$freq
    if (is.null(edge_freq) || all(is.na(edge_freq))) {
      edge_freq <- rep(1, nrow(edges_df))
    }
    edge_freq <- as.numeric(edge_freq)

    edges <- data.frame(
      id = seq_len(nrow(edges_df)),
      from = edges_df$from,
      to = edges_df$to,
      value = scales::rescale(edge_freq, to = c(1, 6)),
      title = paste0("freq: ", round(edge_freq, 3)),
      color = "rgba(120,120,120,0.5)",
      stringsAsFactors = FALSE
    )

    list(nodes = nodes, edges = edges)
  })

  pcsf_network_styled_data <- reactive({
    base <- pcsf_network_base_data()
    nodes <- base$nodes
    edges <- base$edges

    selected_id <- rv$pcsf_selected_node
    if (is.null(selected_id) || !nzchar(selected_id) || !(selected_id %in% nodes$id)) {
      return(list(nodes = nodes, edges = edges, selected_id = NULL))
    }

    g <- rv$annotation_object$pcsf_subnet
    neighbor_ids <- names(igraph::neighbors(g, selected_id, mode = "all"))
    active_ids <- unique(c(selected_id, neighbor_ids))

    is_selected <- nodes$id == selected_id
    is_active <- nodes$id %in% active_ids

    inactive_fill <- "#d0d5dd"
    nodes$font.color <- ifelse(
      is_selected,
      "#8b1e1e",
      ifelse(
        is_active,
        "#475467",
        "#b8c1cc"
      )
    )
    nodes$font.face <- ifelse(is_selected, "Arial Black", "Arial")
    nodes$font.size <- 14
    nodes$color.background <- ifelse(is_active, nodes$color.background, inactive_fill)
    nodes$color.border <- ifelse(
      is_selected,
      "#111111",
      ifelse(is_active, nodes$color.border, inactive_fill)
    )
    nodes$borderWidth <- ifelse(is_selected, 4, 0)
    nodes$color.highlight.background <- nodes$color.background
    nodes$color.highlight.border <- nodes$color.border
    nodes$color.hover.background <- nodes$color.background
    nodes$color.hover.border <- nodes$color.border

    active_edges <- (edges$from == selected_id & edges$to %in% neighbor_ids) |
      (edges$to == selected_id & edges$from %in% neighbor_ids)
    edges$color <- ifelse(active_edges, "rgba(120,120,120,0.65)", "rgba(200,200,200,0.12)")

    list(nodes = nodes, edges = edges, selected_id = selected_id)
  })

  output$pcsf_network_plot <- renderVisNetwork({
    graph_data <- pcsf_network_base_data()

    visNetwork(graph_data$nodes, graph_data$edges, width = "100%", height = "520px") %>%
      visNodes(
        labelHighlightBold = TRUE,
        borderWidth = 0,
        borderWidthSelected = 0,
        font = list(
          size = 14,
          face = "Arial",
          color = "#475467"
        ),
        scaling = list(
          min = 10,
          max = 40,
          label = list(
            enabled = FALSE,
            min = 14,
            max = 14
          )
        )
      ) %>%
      visInteraction(
        hoverConnectedEdges = FALSE,
        selectConnectedEdges = FALSE
      ) %>%
      visEvents(
        click = "function(params) {
          var selected = params.nodes && params.nodes.length ? params.nodes[0] : null;
          Shiny.setInputValue('pcsf_network_selected_node', selected, {priority: 'event'});
        }"
      ) %>%
      visPhysics(stabilization = FALSE)
  })

  observeEvent(list(rv$pcsf_selected_node, rv$pcsf_status, input$pcsf_show_network), {
    if (!(rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network))) {
      return()
    }

    graph_data <- pcsf_network_styled_data()
    proxy <- visNetworkProxy("pcsf_network_plot")

    visUpdateNodes(proxy, graph_data$nodes) %>%
      visUpdateEdges(graph_data$edges)

    visUnselectAll(proxy)
  }, ignoreInit = FALSE)

  observeEvent(input$pcsf_network_selected_node, {
    selected_id <- input$pcsf_network_selected_node

    rv$pcsf_selected_node <- selected_id
  }, ignoreNULL = FALSE)

  observeEvent(rv$pcsf_selected_node, {
    if (!(rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network))) {
      return()
    }

    selected <- pcsf_selected_node_info()

    freezeReactiveValue(input, "pcsf_feature_select")
    freezeReactiveValue(input, "pcsf_metabolite_select")

    if (is.null(selected)) {
      updateSelectizeInput(session, "pcsf_feature_select", selected = character(0))
      updateSelectizeInput(session, "pcsf_metabolite_select", selected = character(0))
      return()
    }

    if (identical(selected$group, "Feature")) {
      updateSelectizeInput(session, "pcsf_feature_select", selected = selected$id)
      updateSelectizeInput(session, "pcsf_metabolite_select", selected = character(0))
      return()
    }

    if (identical(selected$group, "Metabolite")) {
      updateSelectizeInput(session, "pcsf_feature_select", selected = character(0))
      updateSelectizeInput(session, "pcsf_metabolite_select", selected = selected$id)
      return()
    }

    updateSelectizeInput(session, "pcsf_feature_select", selected = character(0))
    updateSelectizeInput(session, "pcsf_metabolite_select", selected = character(0))
  }, ignoreInit = FALSE)

  observeEvent(input$pcsf_feature_select, {
    selected_id <- input$pcsf_feature_select

    if (is.null(selected_id) || !nzchar(selected_id)) {
      current_selected <- pcsf_selected_node_info()
      if (!is.null(current_selected) && identical(current_selected$group, "Metabolite")) {
        return()
      }

      if (!is.null(rv$pcsf_selected_node)) {
        rv$pcsf_selected_node <- NULL
      }
      return()
    }

    rv$pcsf_selected_node <- as.character(selected_id)
  }, ignoreInit = TRUE)

  observeEvent(input$pcsf_metabolite_select, {
    selected_id <- input$pcsf_metabolite_select

    if (is.null(selected_id) || !nzchar(selected_id)) {
      current_selected <- pcsf_selected_node_info()
      if (!is.null(current_selected) && identical(current_selected$group, "Feature")) {
        return()
      }

      if (!is.null(rv$pcsf_selected_node)) {
        rv$pcsf_selected_node <- NULL
      }
      return()
    }

    rv$pcsf_selected_node <- as.character(selected_id)
  }, ignoreInit = TRUE)

  output$pcsf_selected_annotation_ui <- renderUI({
    tags$div(
      class = "pcsf-linked-table-card",
      tags$hr(),
      tags$div(
        class = "annotation-table-wrap",
        DTOutput("pcsf_selected_annotation_table")
      )
    )
  })

  output$pcsf_selected_annotation_table <- renderDT({
    if (rv$pcsf_status != "finished") {
      return(render_table_message("Select a node, and the corresponding annotation results will be shown here."))
    }

    selected <- pcsf_selected_node_info()
    if (is.null(selected)) {
      return(render_table_message("Select a node, and the corresponding annotation results will be shown here."))
    }

    filtered <- pcsf_selected_annotation_data()
    if (is.null(filtered) || nrow(filtered) == 0) {
      return(render_table_message("No Annotation Table 2 rows linked to the selected node."))
    }

    datatable(
      filtered,
      options = list(
        dom = "tip",
        pageLength = 3,
        lengthChange = FALSE,
        scrollX = TRUE,
        scrollY = "150px"
      ),
      rownames = FALSE
    )
  })
  
  output$annotation_table2 <- renderDT({
    if (is.null(rv$annotation_table2) || nrow(rv$annotation_table2) == 0) {
      return(render_table_message("No annotation table 2 results yet"))
    }
    
    datatable(
      rv$annotation_table2,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  observeEvent(input$next_to_modules, {
    req(rv$pcsf_status == "finished")
    switch_page("modules")
  })
  
 
  ## Step 4: Module Identification =====================================
  observeEvent(input$run_modules, {
    req(rv$annotation_object)
    req(rv$pcsf_status == "finished")
    show_process_modal(
      title = "Step 4 Running",
      text = "Module identification is in progress. Please wait."
    )
    rv$module_status <- "running"
    shinyjs::disable("run_modules")
    shinyjs::delay(100, {
      res <- tryCatch({
        identify_module(
          res        = rv$annotation_object,
          method     = input$module_method,
          min_size   = input$min_module_size,
          max_size   = input$max_module_size,
          n_perm     = input$n_perm,
          seed       = input$module_seed,
          workers    = input$module_workers,
          node_freq  = input$node_freq,
          louvain_resolution = if (input$module_method == "louvain") {
            input$louvain_res
          } else {
            1
          },
          walktrap_steps = if (input$module_method == "walktrap") {
            input$walktrap_steps
          } else {
            4
          }
        )
      }, error = function(e) e)
      
      if (inherits(res, "error")) {
        rv$module_status <- "idle"
        shinyjs::enable("run_modules")
        showNotification(res$message, type = "error", duration = NULL)
        update_process_modal("error", "Step 4 Failed", res$message)
        close_process_modal(1800)
      } else {
        rv$module_res    <- res
        rv$mrn           <- res$mrn
        rv$module_status <- "finished"
        update_process_modal(
          "success",
          "Step 4 Completed",
          "Module identification finished successfully."
        )
        close_process_modal(900)
      }
      
      
      shinyjs::enable("run_modules")
    })
  })
  output$module_network_ui <- renderUI({
    if (rv$module_status == "running") {
      tags$div(
        class = "empty-panel empty-panel-fill",
        icon("spinner", class = "fa-spin"),
        "Identifying modules..."
      )
    } else if (rv$module_status == "finished" && !is.null(input$selected_module)) {
      visNetworkOutput("module_network", height = "100%", width = "100%")
    } else {
      tags$div(
        class = "empty-panel empty-panel-fill",
        icon("circle-info"),
        "Module has not been identified yet."
      )
    }
  })
  
  output$module_network_legend <- renderUI({
    if (!(rv$module_status == "finished" && !is.null(input$selected_module))) {
      return(NULL)
    }
    tags$div(
      class = "network-legend-row",
      tags$span(
        class = "network-legend-item",
        tags$span(class = "network-legend-dot", style = "background:#2a9d8f;"),
        "Annotated"
      ),
      tags$span(
        class = "network-legend-item",
        tags$span(class = "network-legend-dot", style = "background:#b56576;"),
        "Hidden"
      )
    )
  })

  module_node_lookup <- reactive({
    if (!(rv$module_status == "finished") ||
        is.null(rv$module_res) ||
        is.null(rv$module_res$module_table) ||
        is.null(input$selected_module)) {
      return(data.frame(
        id = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      ))
    }

    tbl <- rv$module_res$module_table %>%
      dplyr::filter(Module == input$selected_module) %>%
      dplyr::distinct(INCHI_ID, Compound_name)

    if (nrow(tbl) == 0) {
      return(data.frame(
        id = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      ))
    }

    node_label <- ifelse(
      !is.na(tbl$Compound_name) & nzchar(tbl$Compound_name),
      as.character(tbl$Compound_name),
      as.character(tbl$INCHI_ID)
    )

    data.frame(
      id = as.character(tbl$INCHI_ID),
      label = node_label,
      stringsAsFactors = FALSE
    )
  })
  
  
  output$module_selector_ui <- renderUI({
    req(rv$module_res)
    req(rv$module_res$module_score)
    
    mods <- unique(as.character(rv$module_res$module_score$Module))
    mod_num <- suppressWarnings(as.numeric(gsub("[^0-9]+", "", mods)))
    mods <- mods[order(is.na(mod_num), mod_num, mods)]
    req(length(mods) > 0)
    current_selected <- input$selected_module
    selected_mod <- if (!is.null(current_selected) && current_selected %in% mods) {
      current_selected
    } else {
      mods[1]
    }
    
    tags$div(
      class = "module-selector-wrap",
      selectInput(
        "selected_module",
        "Select module to visualize",
        choices  = mods,
        selected = selected_mod,
        width = "100%"
      )
    )
  })

  observeEvent(list(rv$module_status, input$selected_module, rv$module_res$module_table), {
    if (rv$module_status != "finished" || is.null(input$selected_module)) {
      updateSelectizeInput(
        session,
        "module_metabolite_select",
        choices = character(0),
        selected = character(0),
        server = TRUE
      )
      return()
    }

    lookup <- module_node_lookup()

    updateSelectizeInput(
      session,
      "module_metabolite_select",
      choices = if (nrow(lookup) > 0) stats::setNames(lookup$id, lookup$label) else character(0),
      selected = character(0),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  observeEvent(input$selected_module, {
    rv$module_selected_node <- NULL

    if ("module_metabolite_select" %in% names(input)) {
      freezeReactiveValue(input, "module_metabolite_select")
      updateSelectizeInput(session, "module_metabolite_select", selected = character(0))
    }

    if (rv$module_status == "finished") {
      visUnselectAll(visNetworkProxy("module_network"))
    }
  }, ignoreInit = TRUE)

  module_network_base_data <- reactive({
    req(rv$module_res)
    req(input$selected_module)

    tbl <- rv$module_res$module_table %>%
      dplyr::filter(Module == input$selected_module)

    req(nrow(tbl) > 1)

    g <- igraph::induced_subgraph(
      rv$mrn,
      vids = tbl$INCHI_ID
    )

    node_id <- igraph::V(g)$name
    node_type <- rv$module_res$module_table %>%
      dplyr::filter(Module == input$selected_module) %>%
      dplyr::select(INCHI_ID, Type, Compound_name)

    type_map <- setNames(node_type$Type, node_type$INCHI_ID)
    label_map <- setNames(node_type$Compound_name, node_type$INCHI_ID)
    node_label_full <- ifelse(
      !is.na(label_map[node_id]) & nzchar(label_map[node_id]),
      label_map[node_id],
      node_id
    )

    nodes <- data.frame(
      id = node_id,
      label = ifelse(
        nchar(node_label_full) > 28,
        paste0(substr(node_label_full, 1, 25), "..."),
        node_label_full
      ),
      title = node_label_full,
      color.background = ifelse(
        type_map[node_id] == "annotated",
        "#2a9d8f",
        "#b56576"
      ),
      color.border = ifelse(
        type_map[node_id] == "annotated",
        "#2a9d8f",
        "#b56576"
      ),
      color.highlight.background = ifelse(
        type_map[node_id] == "annotated",
        "#2a9d8f",
        "#b56576"
      ),
      color.highlight.border = ifelse(
        type_map[node_id] == "annotated",
        "#2a9d8f",
        "#b56576"
      ),
      color.hover.background = ifelse(
        type_map[node_id] == "annotated",
        "#2a9d8f",
        "#b56576"
      ),
      color.hover.border = ifelse(
        type_map[node_id] == "annotated",
        "#2a9d8f",
        "#b56576"
      ),
      font.color = "#475467",
      font.face = "Arial",
      font.size = 14,
      borderWidth = 0,
      stringsAsFactors = FALSE
    )

    edges_df <- igraph::as_data_frame(g, what = "edges")
    edge_from <- as.character(edges_df$from)
    edge_to <- as.character(edges_df$to)
    edge_id <- paste(pmin(edge_from, edge_to), pmax(edge_from, edge_to), sep = "__")

    edges <- data.frame(
      id = edge_id,
      from = edge_from,
      to = edge_to,
      color = "rgba(120,120,120,0.5)",
      stringsAsFactors = FALSE
    )

    list(nodes = nodes, edges = edges)
  })

  module_network_styled_data <- reactive({
    base <- module_network_base_data()
    nodes <- base$nodes
    edges <- base$edges
    selected_id <- rv$module_selected_node

    if (is.null(selected_id) || !nzchar(selected_id) || !(selected_id %in% nodes$id)) {
      return(list(nodes = nodes, edges = edges))
    }

    tbl <- rv$module_res$module_table %>%
      dplyr::filter(Module == input$selected_module)
    g <- igraph::induced_subgraph(
      rv$mrn,
      vids = tbl$INCHI_ID
    )
    neighbor_ids <- names(igraph::neighbors(g, selected_id, mode = "all"))
    active_ids <- unique(c(selected_id, neighbor_ids))

    is_selected <- nodes$id == selected_id
    is_active <- nodes$id %in% active_ids
    inactive_fill <- "#d0d5dd"

    nodes$color.background <- ifelse(is_active, nodes$color.background, inactive_fill)
    nodes$color.border <- ifelse(is_active, nodes$color.border, inactive_fill)
    nodes$color.highlight.background <- nodes$color.background
    nodes$color.highlight.border <- nodes$color.border
    nodes$color.hover.background <- nodes$color.background
    nodes$color.hover.border <- nodes$color.border
    nodes$font.color <- ifelse(
      is_selected,
      "#8b1e1e",
      ifelse(is_active, "#475467", "#b8c1cc")
    )
    nodes$font.color <- ifelse(is_selected, "#8b1e1e", nodes$font.color)
    nodes$font.face <- ifelse(is_selected, "Arial Black", nodes$font.face)
    nodes$color.border <- ifelse(is_selected, "#111111", nodes$color.border)
    nodes$color.highlight.border <- nodes$color.border
    nodes$color.hover.border <- nodes$color.border
    nodes$borderWidth <- ifelse(is_selected, 4, 0)

    active_edges <- (edges$from == selected_id & edges$to %in% neighbor_ids) |
      (edges$to == selected_id & edges$from %in% neighbor_ids)
    edges$color <- ifelse(active_edges, "rgba(120,120,120,0.65)", "rgba(200,200,200,0.12)")

    list(nodes = nodes, edges = edges)
  })
  
  output$module_network <- renderVisNetwork({
    graph_data <- module_network_base_data()

    visNetwork(graph_data$nodes, graph_data$edges, width = "100%", height = "700px") %>%
      visNodes(
        borderWidth = 0,
        borderWidthSelected = 0,
        font = list(size = 14, face = "Arial", color = "#475467"),
        scaling = list(
          label = list(enabled = FALSE)
        )
      ) %>%
      visInteraction(
        hoverConnectedEdges = FALSE,
        selectConnectedEdges = FALSE
      ) %>%
      visEvents(
        click = "function(params) {
          var selected = params.nodes && params.nodes.length ? params.nodes[0] : null;
          Shiny.setInputValue('module_network_selected_node', selected, {priority: 'event'});
        }"
      ) %>%
      visPhysics(stabilization = FALSE)
    
  })

  observeEvent(input$module_network_selected_node, {
    rv$module_selected_node <- input$module_network_selected_node
  }, ignoreNULL = FALSE)

  observeEvent(input$module_metabolite_select, {
    selected_id <- input$module_metabolite_select

    if (is.null(selected_id) || !nzchar(selected_id)) {
      if (!is.null(rv$module_selected_node)) {
        rv$module_selected_node <- NULL
      }
      return()
    }

    rv$module_selected_node <- as.character(selected_id)
  }, ignoreInit = TRUE)

  observeEvent(rv$module_selected_node, {
    if (!(rv$module_status == "finished" && !is.null(input$selected_module))) {
      return()
    }

    freezeReactiveValue(input, "module_metabolite_select")

    lookup <- module_node_lookup()
    if (is.null(rv$module_selected_node) || !(rv$module_selected_node %in% lookup$id)) {
      updateSelectizeInput(session, "module_metabolite_select", selected = character(0))
      visUnselectAll(visNetworkProxy("module_network"))
      return()
    }

    updateSelectizeInput(session, "module_metabolite_select", selected = rv$module_selected_node)
    visNetworkProxy("module_network") %>%
      visSelectNodes(id = rv$module_selected_node)
  }, ignoreInit = FALSE)

  observeEvent(list(rv$module_selected_node, rv$module_status, input$selected_module), {
    if (!(rv$module_status == "finished" && !is.null(input$selected_module))) {
      return()
    }

    graph_data <- module_network_styled_data()
    proxy <- visNetworkProxy("module_network")

    visUpdateNodes(proxy, graph_data$nodes) %>%
      visUpdateEdges(graph_data$edges)
    visUnselectAll(proxy)
  }, ignoreInit = FALSE)
  
  output$module_score <- renderDT({
    if (is.null(rv$module_res) ||
        is.null(rv$module_res$module_score) ||
        nrow(rv$module_res$module_score) == 0) {
      return(render_table_message("No module score results yet"))
    }
    
    datatable(
      rv$module_res$module_score,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$module_table <- renderDT({
    if (is.null(rv$module_res) ||
        is.null(rv$module_res$module_table) ||
        nrow(rv$module_res$module_table) == 0) {
      return(render_table_message("No module membership results yet"))
    }
    
    datatable(
      rv$module_res$module_table,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  output$module_score_status_ui <- renderUI({
    if (rv$module_status == "running") {
      return(
        tags$div(
          class = "param-card",
          tags$h4(icon("spinner", class = "fa-spin"), "Module Score Status"),
          tags$p(style = "margin-bottom:0;", "Module scoring is running...")
        )
      )
    }

    if (is.null(rv$module_res) ||
        is.null(rv$module_res$module_score) ||
        nrow(rv$module_res$module_score) == 0) {
      return(
        tags$div(
          class = "param-card",
          tags$h4(icon("circle-info"), "Module Score Status"),
          tags$p(style = "margin-bottom:0;", "No module score results yet.")
        )
      )
    }

    score_tbl <- rv$module_res$module_score
    module_col <- pick_first_col(score_tbl, c("Module", "module", "module_id"))
    p_col <- pick_first_col(score_tbl, c("p", "pvalue", "p_value", "P", "P.Value"))
    fdr_col <- pick_first_col(score_tbl, c("fdr", "FDR", "qvalue", "q_value", "padj", "p_adj"))

    n_modules <- if (!is.null(module_col)) {
      length(unique(stats::na.omit(score_tbl[[module_col]])))
    } else {
      nrow(score_tbl)
    }

    n_p_sig <- NA_integer_
    if (!is.null(p_col)) {
      p_vals <- suppressWarnings(as.numeric(score_tbl[[p_col]]))
      n_p_sig <- sum(is.finite(p_vals) & p_vals < 0.05, na.rm = TRUE)
    }

    n_fdr_sig <- NA_integer_
    if (!is.null(fdr_col)) {
      fdr_vals <- suppressWarnings(as.numeric(score_tbl[[fdr_col]]))
      n_fdr_sig <- sum(is.finite(fdr_vals) & fdr_vals < 0.05, na.rm = TRUE)
    }

    tags$div(
      class = "param-card",
      tags$h4(icon("circle-check"), "Module Score Status"),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Modules scored: ", n_modules)
      ),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Modules with p < 0.05: ", ifelse(is.na(n_p_sig), "NA", n_p_sig))
      ),
      tags$p(
        style = "margin-bottom:0;",
        paste0("Modules with FDR < 0.05: ", ifelse(is.na(n_fdr_sig), "NA", n_fdr_sig))
      )
    )
  })

  output$module_table_status_ui <- renderUI({
    if (rv$module_status == "running") {
      return(
        tags$div(
          class = "param-card",
          tags$h4(icon("spinner", class = "fa-spin"), "Module Table Status"),
          tags$p(style = "margin-bottom:0;", "Module table is being generated...")
        )
      )
    }

    if (is.null(rv$module_res) ||
        is.null(rv$module_res$module_table) ||
        nrow(rv$module_res$module_table) == 0) {
      return(
        tags$div(
          class = "param-card",
          tags$h4(icon("circle-info"), "Module Table Status"),
          tags$p(style = "margin-bottom:0;", "No module table results yet.")
        )
      )
    }

    tbl <- rv$module_res$module_table
    module_col <- pick_first_col(tbl, c("Module", "module", "module_id"))
    node_col <- pick_first_col(tbl, c("INCHI_ID", "node", "node_id", "id", "name"))
    type_col <- pick_first_col(tbl, c("Type", "type"))

    n_modules <- if (!is.null(module_col)) {
      length(unique(stats::na.omit(tbl[[module_col]])))
    } else {
      NA_integer_
    }

    n_nodes <- if (!is.null(node_col)) {
      length(unique(stats::na.omit(tbl[[node_col]])))
    } else {
      NA_integer_
    }

    annotated_hidden_txt <- "NA"
    if (!is.null(type_col)) {
      tp <- tolower(as.character(tbl[[type_col]]))
      annotated_hidden_txt <- paste0(
        sum(tp == "annotated", na.rm = TRUE),
        " / ",
        sum(tp %in% c("hidden", "unannotated"), na.rm = TRUE)
      )
    }

    tags$div(
      class = "param-card",
      tags$h4(icon("circle-check"), "Module Table Status"),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Rows: ", nrow(tbl))
      ),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Modules: ", ifelse(is.na(n_modules), "NA", n_modules),
               "; Unique nodes: ", ifelse(is.na(n_nodes), "NA", n_nodes))
      ),
      tags$p(
        style = "margin-bottom:0;",
        paste0("Annotated / Hidden: ", annotated_hidden_txt)
      )
    )
  })
  
  observeEvent(input$next_to_function, {
    req(rv$module_status == "finished")
    switch_page("function")
  })
  
  
  ## Step 5: Functional Annotation =====================================
  observeEvent(input$run_function_annotation, {
    
    req(rv$module_res)
    req(rv$module_status == "finished")
    
    # ---- basic checks ----
    req(nzchar(input$func_llm_model))
    
    llm_key <- trimws(input$func_llm_key)
    if (!nzchar(llm_key)) {
      llm_key <- trimws(Sys.getenv("FEATUREMAP_LLM_API_KEY", unset = ""))
    }
    if (!nzchar(llm_key)) {
      showNotification(
        "LLM API key missing. Fill the input or set FEATUREMAP_LLM_API_KEY.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    if (isTRUE(input$func_use_pubmed)) {
      req(nzchar(input$func_embed_model))
      ncbi_key <- trimws(input$func_ncbi_key)
      if (!nzchar(ncbi_key)) {
        ncbi_key <- trimws(Sys.getenv("FEATUREMAP_NCBI_API_KEY", unset = ""))
      }
      if (!nzchar(ncbi_key)) {
        showNotification(
          "NCBI API key missing. Fill the input or set FEATUREMAP_NCBI_API_KEY.",
          type = "error",
          duration = NULL
        )
        return()
      }
    } else {
      ncbi_key <- trimws(input$func_ncbi_key)
      if (!nzchar(ncbi_key)) {
        ncbi_key <- trimws(Sys.getenv("FEATUREMAP_NCBI_API_KEY", unset = ""))
      }
    }
    
    ref_data <- tryCatch({
      
      if (input$func_class_source == "default") {
        NULL
        
      } else {
        
        req(input$func_class_file)
        
        cat("Loading uploaded file:", input$func_class_file$name, "\n")
        
        ext <- tools::file_ext(input$func_class_file$name)
        
        result <- if (ext %in% c("rda", "RData")) {
          e <- new.env()
          load(input$func_class_file$datapath, envir = e)
          e[[ls(e)[1]]]
        } else {
          read.csv(input$func_class_file$datapath, check.names = FALSE)
        }
        
        cat("Uploaded data dimensions:", nrow(result), "x", ncol(result), "\n")
        result
      }
      
    }, error = function(e) {
      cat("!!! ERROR loading ref_data:", e$message, "\n")
      showNotification(
        paste("Failed to load reference data:", e$message),
        type = "error",
        duration = NULL
      )
      NULL
    })

    if (input$func_class_source != "default" && is.null(ref_data)) {
      cat("ref_data is NULL, aborting\n")
      return()
    }

    show_process_modal(
      title = "Step 5 Running",
      text = "Module annotation is in progress. Please wait."
    )
    
    # ---- set status ----
    rv$function_status <- "running"
    rv$function_pubmed_used <- isTRUE(input$func_use_pubmed)
    shinyjs::disable("run_function_annotation")
    
    shinyjs::delay(100, {
      
      res <- tryCatch({
        
        # ---- run annotation ----
        result <- annotate_module(
          res           = rv$module_res,
          p             = input$func_p_type,
          pcutoff       = input$func_p_cutoff,
          ref_data      = ref_data,
          species       = {
            x <- trimws(input$func_species)
            if (nzchar(x)) x else ""
          } ,
          biospecimen = {
            x <- trimws(input$func_biospecimen)
            if (nzchar(x)) x else ""
          },
          phenotype = {
            x <- trimws(input$func_phenotype)
            if (nzchar(x)) x else ""
          },
          pubmed        = input$func_use_pubmed,
          years         = input$func_pubmed_years,
          top_paper     = input$func_top_paper,
          min_similarity= input$func_min_similarity,
          provider      = input$func_provider,
          embed_model   = input$func_embed_model,
          llm_model     = input$func_llm_model,
          NCBI_api_key  = ncbi_key,
          llm_api_key   = llm_key
        )
        
        result
        
      }, error = function(e) {
        e
      })
      
      # ---- handle result ----
      if (inherits(res, "error")) {
        cat("Error detected, reverting status\n")
        rv$function_status <- "error"
        shinyjs::enable("run_function_annotation")
        
        showNotification(
          paste("Functional annotation failed:", res$message),
          type = "error",
          duration = NULL
        )
        update_process_modal(
          "error",
          "Step 5 Failed",
          paste("Functional annotation failed:", res$message)
        )
        close_process_modal(2200)
        
      } else {
        # ---- normalize empty result as "finished" ----
        mf <- res$module_function
        is_empty <- is.null(mf) || !is.data.frame(mf) || nrow(mf) == 0
        
        rv$function_result <- res
        rv$function_status <- "finished"
        
        if (is_empty) {
          showNotification(
            "No significant modules passed the cutoff. Try relaxing the cutoff or using p-value instead of FDR.",
            type = "warning",
            duration = NULL
          )
        } else {
          # ---- force selector refresh ----
          if ("module_id" %in% names(mf)) {
            module_ids <- sort(unique(mf$module_id))
            updateSelectInput(
              session,
              "selected_function_module",
              choices  = module_ids,
              selected = module_ids[1]
            )
          }
        }
        
        update_process_modal(
          "success",
          "Step 5 Completed",
          "Module function annotation finished successfully."
        )
        close_process_modal(900)
        
        shinyjs::enable("run_function_annotation")
      }
    })
  })
  
  output$function_main_ui <- renderUI({
    
    # ---- running ----
    if (rv$function_status == "running") {
      tags$div(
        class = "empty-panel empty-panel-fill",
        icon("spinner", class = "fa-spin"),
        "Generating AI-powered functional annotation..."
      )
      
      # ---- finished but no result ----
    } else if (
      rv$function_status == "finished" &&
      (is.null(rv$function_result) ||
       is.null(rv$function_result$module_function) ||
       nrow(rv$function_result$module_function) == 0)
    ) {
      
      tags$div(
        class = "empty-panel empty-panel-fill",
        icon("triangle-exclamation"),
        "No significant modules passed the cutoff."
      )
      
      # ---- finished with result ----
    } else if (rv$function_status == "finished") {
      
      tags$div(
        class = "function-result-shell",
        uiOutput("function_module_selector"),
        uiOutput("function_module_output"),
        uiOutput("function_literature_ui"),
        uiOutput("function_result_status_ui")
      )
      
      # ---- idle ----
    } else {
      
      tags$div(
        class = "empty-panel empty-panel-fill",
        icon("robot"),
        "Click Run to annotate module functions."
      )
    }
  })

  #### ---- Module selector ----
  output$function_module_selector <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (is.null(rv$function_result) || is.null(rv$function_result$module_function)) return(NULL)
    
    df <- rv$function_result$module_function
    if (!is.data.frame(df) || !"module_id" %in% colnames(df)) return(NULL)
    
    module_ids <- sort(unique(df$module_id))
    if (length(module_ids) == 0) return(NULL)
    
    tags$div(
      class = "function-selector-wrap",
      selectInput(
        "selected_function_module",
        "Select module",
        choices  = module_ids,
        selected = module_ids[1],
        width = "100%"
      )
    )
  })
  
  
  #### ---- Module annotation output ----
  output$function_literature_ui <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (is.null(input$selected_function_module)) return(NULL)
    if (is.null(rv$function_result)) return(NULL)

    pubmed_res <- rv$function_result$module_pubmed_embed_refined
    if (!isTRUE(rv$function_pubmed_used)) {
      return(
        tags$div(
          class = "param-card function-annotation-card",
          tags$h4(icon("book"), "Literature Evidence (PMID)"),
          tags$p(
            class = "function-annotation-note",
            "Literature-based annotation was disabled in this run."
          )
        )
      )
    }
    if (is.null(pubmed_res) || length(pubmed_res) == 0) {
      return(
        tags$div(
          class = "param-card function-annotation-card",
          tags$h4(icon("book"), "Literature Evidence (PMID)"),
          tags$p(
            class = "function-annotation-note",
            "No literature evidence object found. Please re-run Step 5 with Literature-based Annotation enabled."
          )
        )
      )
    }

    module_id_txt <- as.character(input$selected_function_module)
    module_id_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", module_id_txt)))
    key_candidates <- unique(c(
      module_id_txt,
      paste0("module", module_id_txt),
      gsub("^module", "", module_id_txt, ignore.case = TRUE),
      paste0("module", gsub("^module", "", module_id_txt, ignore.case = TRUE)),
      if (is.finite(module_id_num)) as.character(module_id_num) else character(0),
      if (is.finite(module_id_num)) paste0("module", module_id_num) else character(0)
    ))

    mod_pub <- NULL
    if (!is.null(names(pubmed_res))) {
      hit <- intersect(key_candidates, names(pubmed_res))
      if (length(hit) > 0) mod_pub <- pubmed_res[[hit[1]]]
      if (is.null(mod_pub) && is.finite(module_id_num)) {
        suffix_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", names(pubmed_res))))
        idx2 <- which(!is.na(suffix_num) & suffix_num == module_id_num)
        if (length(idx2) > 0) mod_pub <- pubmed_res[[idx2[1]]]
      }
    }
    if (is.null(mod_pub)) {
      idx <- if (is.finite(module_id_num)) module_id_num else suppressWarnings(as.integer(module_id_txt))
      if (is.finite(idx) && idx >= 1 && idx <= length(pubmed_res)) {
        mod_pub <- pubmed_res[[idx]]
      }
    }

    if (is.null(mod_pub) || length(mod_pub) == 0) {
      return(
        tags$div(
          class = "param-card function-annotation-card",
          tags$h4(icon("book"), "Literature Evidence (PMID)"),
          tags$p(
            class = "function-annotation-note",
            "No PubMed evidence matched to the selected module."
          )
        )
      )
    }

    collect_pmids <- function(x) {
      out <- character(0)
      if (is.list(x)) {
        if (!is.null(x$pmids)) out <- c(out, as.character(x$pmids))
        for (i in seq_along(x)) out <- c(out, collect_pmids(x[[i]]))
      }
      out
    }

    collect_compound_pmids <- function(mod_pub) {
      if (!is.list(mod_pub)) return(list())
      mod_names <- names(mod_pub)
      out <- list()

      for (i in seq_along(mod_pub)) {
        ent <- mod_pub[[i]]
        if (!is.list(ent) || (is.null(ent$pmids) && is.null(ent$met_name))) next

        pmids_i <- as.character(ent$pmids %||% character(0))
        pmids_i <- unique(trimws(pmids_i))
        pmids_i <- pmids_i[!is.na(pmids_i) & nzchar(pmids_i)]

        ent_id <- ""
        if (!is.null(mod_names) && length(mod_names) >= i && !is.na(mod_names[i])) {
          ent_id <- trimws(as.character(mod_names[i]))
        }
        if (!nzchar(ent_id)) ent_id <- as.character(i)

        ent_name <- trimws(as.character(ent$met_name %||% ""))
        ent_name <- ent_name[!is.na(ent_name) & nzchar(ent_name)]
        ent_name <- if (length(ent_name) > 0) ent_name[1] else ent_id

        out[[length(out) + 1]] <- list(
          compound_id = ent_id,
          compound_name = ent_name,
          pmids = pmids_i,
          n_pmids = length(pmids_i)
        )
      }

      out
    }

    compound_pmids <- collect_compound_pmids(mod_pub)
    pmids <- if (length(compound_pmids) > 0) {
      unique(unlist(lapply(compound_pmids, `[[`, "pmids"), use.names = FALSE))
    } else {
      unique(collect_pmids(mod_pub))
    }
    pmids <- pmids[!is.na(pmids) & nzchar(trimws(pmids))]

    if (length(pmids) == 0) {
      return(
        tags$div(
          class = "param-card function-annotation-card",
          tags$h4(icon("book"), "Literature Evidence (PMID)"),
          tags$p(class = "function-annotation-note", "No retained PubMed references for this module.")
        )
      )
    }

    if (length(compound_pmids) > 0) {
      n_compounds <- length(compound_pmids)
      n_compounds_with_hits <- sum(vapply(compound_pmids, function(x) x$n_pmids > 0, logical(1)))
      ord <- order(vapply(compound_pmids, function(x) x$n_pmids, integer(1)), decreasing = TRUE)
      compound_pmids <- compound_pmids[ord]

      make_pmid_cell <- function(pmids_vec) {
        if (length(pmids_vec) == 0) return(tags$span("-"))
        if (length(pmids_vec) == 1) {
          x <- pmids_vec[1]
          return(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", x, "/"), target = "_blank", rel = "noopener noreferrer", x))
        }
        parts <- list()
        for (i in seq_along(pmids_vec)) {
          x <- pmids_vec[i]
          parts[[length(parts) + 1]] <- tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", x, "/"), target = "_blank", rel = "noopener noreferrer", x)
          if (i < length(pmids_vec)) parts[[length(parts) + 1]] <- tags$span(", ")
        }
        do.call(tagList, parts)
      }

      return(
        tags$div(
          class = "param-card function-annotation-card",
          tags$h4(icon("book"), "Literature Evidence (PMID)"),
          tags$p(
            class = "function-annotation-note",
            paste0(
              "Top compounds with retained references: ",
              n_compounds_with_hits, "/", n_compounds,
              " | Unique PMIDs: ", length(pmids)
            )
          ),
          tags$div(
            class = "table-responsive",
            tags$table(
              class = "table table-sm table-striped table-bordered",
              style = "margin-bottom:0;",
              tags$thead(
                tags$tr(
                  tags$th("Top Compound"),
                  tags$th("Compound ID"),
                  tags$th("PMID Count"),
                  tags$th("PMIDs")
                )
              ),
              tags$tbody(
                lapply(compound_pmids, function(cp) {
                  tags$tr(
                    tags$td(cp$compound_name),
                    tags$td(cp$compound_id),
                    tags$td(as.character(cp$n_pmids)),
                    tags$td(
                      style = "min-width:280px; white-space:normal; word-break:break-word;",
                      make_pmid_cell(cp$pmids)
                    )
                  )
                })
              )
            )
          )
        )
      )
    }

    tags$div(
      class = "param-card function-annotation-card",
      tags$h4(icon("book"), "Literature Evidence (PMID)"),
      tags$p(
        class = "function-annotation-note",
        paste0("Retained references for this module: ", length(pmids))
      ),
      tags$div(
        class = "function-pmid-list",
        lapply(pmids, function(x) {
          tags$a(
            class = "function-pmid-chip",
            href = paste0("https://pubmed.ncbi.nlm.nih.gov/", x, "/"),
            target = "_blank",
            rel = "noopener noreferrer",
            paste0("PMID: ", x)
          )
        })
      )
    )
  })
  output$function_module_output <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (is.null(rv$function_result) || is.null(rv$function_result$module_function)) return(NULL)
    if (is.null(input$selected_function_module)) return(NULL)
    
    mod_df <- rv$function_result$module_function %>%
      dplyr::filter(as.character(module_id) == as.character(input$selected_function_module))
    
    req(nrow(mod_df) == 1)
    
    tags$div(
      class = "function-annotation-grid",
      tags$div(
        class = "param-card function-annotation-card",
        tags$h4(icon("tag"), "Module Title"),
        tags$p(class = "function-annotation-title", mod_df$title)
      ),
      
      tags$div(
        class = "param-card function-annotation-card",
        tags$h4(icon("align-left"), "Functional Summary"),
        tags$p(class = "function-annotation-text", mod_df$summary)
      ),
      
      tags$div(
        class = "param-card function-annotation-card",
        tags$h4(icon("chart-simple"), "Confidence"),
        tags$p(class = "function-annotation-confidence", mod_df$confidence_label)
      ),
      
      tags$div(
        class = "param-card function-annotation-card",
        tags$h4(icon("think-peaks"), "Rationale"),
        tags$p(class = "function-annotation-text", mod_df$rationale)
      )
    )
  })

  output$function_result_status_ui <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (is.null(rv$function_result) || is.null(rv$function_result$module_function)) return(NULL)

    mf <- rv$function_result$module_function
    if (!is.data.frame(mf) || nrow(mf) == 0) return(NULL)

    n_modules <- if ("module_id" %in% names(mf)) {
      length(unique(stats::na.omit(mf$module_id)))
    } else {
      nrow(mf)
    }

    n_summary_ready <- if ("summary" %in% names(mf)) {
      sum(!is.na(mf$summary) & nzchar(trimws(mf$summary)))
    } else {
      n_modules
    }

    has_literature <- !is.null(rv$function_result$module_pubmed_embed_refined) &&
      length(rv$function_result$module_pubmed_embed_refined) > 0

    tags$div(
      class = "param-card",
      tags$h4(icon("circle-check"), "Functional Annotation Status"),
      tags$p(
        style = "margin-bottom:6px;",
        paste0("Annotated modules: ", n_modules)
      ),
      tags$p(
        style = "margin-bottom:6px;",
        paste0(
          "Literature-based annotation: ",
          if (has_literature) "Enabled" else "Disabled"
        )
      ),
      tags$p(
        style = "margin-bottom:0; font-weight:600;",
        "Congratulations! Functional annotation is complete. You can go to Results to download outputs."
      )
    )
  })
  
  observeEvent(input$next_to_results, {
    switch_page("results")
  })
  
  
  ## Step 6: Download the results ================================
  export_labels <- c(
    anno1 = "Annotation table 1",
    anno2 = "Annotation table 2",
    module_score = "Module score",
    module_table = "Module table",
    module_function = "Module function"
  )

  is_export_ready <- function(x) {
    if (is.null(x)) return(FALSE)
    if (is.data.frame(x) || is.matrix(x)) return(TRUE)
    if (is.list(x)) return(length(x) > 0)
    TRUE
  }

  export_payload <- reactive({
    list(
      anno1 = rv$annotated_table,
      anno2 = rv$annotation_table2,
      module_score = rv$module_res$module_score,
      module_table = rv$module_res$module_table,
      module_function = rv$function_result$module_function
    )
  })

  ready_export_items <- reactive({
    payload <- export_payload()
    names(payload)[vapply(payload, is_export_ready, logical(1))]
  })

  ready_network_export_items <- reactive({
    c(
      if (!is.null(pcsf_export_data())) "pcsf",
      if (length(results_module_choices()) > 0) "module"
    )
  })

  observeEvent(input$export_items, ignoreInit = TRUE, {
    if (isTRUE(rv$results_export_tables_updating)) {
      rv$results_export_tables_updating <- FALSE
      return()
    }
    rv$results_export_tables_touched <- TRUE
  })

  observeEvent(input$network_export_items, ignoreInit = TRUE, {
    if (isTRUE(rv$results_export_networks_updating)) {
      rv$results_export_networks_updating <- FALSE
      return()
    }
    rv$results_export_networks_touched <- TRUE
  })

  observe({
    ready_items <- ready_export_items()
    if (isTRUE(rv$results_export_tables_touched)) {
      return()
    }

    current <- input$export_items
    current <- if (is.null(current)) character(0) else current

    if (!setequal(current, ready_items)) {
      rv$results_export_tables_updating <- TRUE
      updateCheckboxGroupInput(session, "export_items", selected = ready_items)
    }
  })

  observe({
    ready_networks <- ready_network_export_items()
    if (isTRUE(rv$results_export_networks_touched)) {
      return()
    }

    current <- input$network_export_items
    current <- if (is.null(current)) character(0) else current

    if (!setequal(current, ready_networks)) {
      rv$results_export_networks_updating <- TRUE
      updateCheckboxGroupInput(session, "network_export_items", selected = ready_networks)
    }
  })

  results_module_choices <- reactive({
    if (is.null(rv$module_res) || is.null(rv$module_res$module_score)) return(character(0))
    mods <- unique(as.character(rv$module_res$module_score$Module))
    mod_num <- suppressWarnings(as.numeric(gsub("[^0-9]+", "", mods)))
    mods <- mods[order(is.na(mod_num), mod_num, mods)]
    mods[!is.na(mods) & nzchar(mods)]
  })

  selected_results_modules <- reactive({
    mods <- results_module_choices()
    if (length(mods) == 0) return(list(mode = "none", requested = character(0)))

    selected <- input$results_export_modules
    if (is.null(selected) || length(selected) == 0 || "__all__" %in% selected) {
      list(mode = "all", requested = mods)
    } else {
      list(mode = "custom", requested = intersect(selected, mods))
    }
  })

  observeEvent(input$results_export_modules, ignoreInit = TRUE, {
    mods <- results_module_choices()
    if (length(mods) == 0) return()

    selected <- input$results_export_modules
    if (is.null(selected) || length(selected) == 0) {
      updateSelectizeInput(session, "results_export_modules", selected = "__all__")
      return()
    }
    if ("__all__" %in% selected && length(selected) > 1) {
      updateSelectizeInput(session, "results_export_modules", selected = "__all__")
    }
  })

  output$results_module_export_selector <- renderUI({
    selected_networks <- input$network_export_items
    if (is.null(selected_networks) || !("module" %in% selected_networks)) {
      return(NULL)
    }

    mods <- results_module_choices()
    if (length(mods) == 0) {
      return(tags$p(class = "results-empty-preview", icon("circle-info"), " Module network is not ready yet."))
    }
    selectizeInput(
      "results_export_modules",
      "Modules For Network Export",
      choices = c("All modules" = "__all__", stats::setNames(mods, mods)),
      selected = "__all__",
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  pcsf_export_data <- reactive({
    if (rv$pcsf_status != "finished") return(NULL)
    if (is.null(rv$annotation_object) || is.null(rv$annotation_object$pcsf_subnet)) return(NULL)

    g <- rv$annotation_object$pcsf_subnet
    if (!igraph::is.igraph(g) || igraph::vcount(g) == 0) return(NULL)

    node_id <- as.character(igraph::V(g)$name)
    node_prize <- as.numeric(igraph::V(g)$prize)
    if (length(node_prize) != length(node_id) || all(is.na(node_prize))) {
      node_prize <- rep(1, length(node_id))
    } else {
      node_prize[is.na(node_prize)] <- stats::median(node_prize, na.rm = TRUE)
      if (!is.finite(node_prize[1])) node_prize <- rep(1, length(node_id))
    }

    node_type <- as.character(igraph::V(g)$type)
    if (length(node_type) != length(node_id) || all(is.na(node_type))) {
      node_type <- rep("Metabolite", length(node_id))
    }
    node_group <- ifelse(node_type == "Terminal", "Feature", "Metabolite")

    node_compound_name <- igraph::V(g)$compound_name
    if (is.null(node_compound_name) || length(node_compound_name) != length(node_id)) {
      node_compound_name <- rep(NA_character_, length(node_id))
    }
    node_compound_name <- as.character(node_compound_name)
    node_label <- ifelse(
      node_group == "Metabolite" & !is.na(node_compound_name) & nzchar(node_compound_name),
      node_compound_name,
      node_id
    )

    nodes <- data.frame(
      id = node_id,
      label = node_label,
      value = scales::rescale(node_prize, to = c(10, 40)),
      color = ifelse(node_group == "Feature", "#fc8d58", "#4474b3"),
      color.border = ifelse(node_group == "Feature", "#fc8d58", "#4474b3"),
      stringsAsFactors = FALSE
    )

    edges_df <- igraph::as_data_frame(g, what = "edges")
    if (nrow(edges_df) > 0) {
      edge_freq <- as.numeric(igraph::E(g)$freq)
      if (length(edge_freq) != nrow(edges_df) || all(is.na(edge_freq))) {
        edge_freq <- rep(1, nrow(edges_df))
      } else {
        edge_freq[is.na(edge_freq)] <- 1
      }
      edge_width <- scales::rescale(edge_freq, to = c(0.8, 2.5))
      edges <- data.frame(
        from = as.character(edges_df$from),
        to = as.character(edges_df$to),
        value = scales::rescale(edge_freq, to = c(1, 6)),
        title = paste0("freq: ", round(edge_freq, 3)),
        color = "rgba(120,120,120,0.5)",
        stringsAsFactors = FALSE
      )
    } else {
      edge_width <- numeric(0)
      edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    }

    list(
      g = g,
      nodes = nodes,
      edges = edges,
      node_group = stats::setNames(node_group, node_id),
      edge_width = edge_width,
      legend_labels = c("Feature", "Metabolite"),
      title = "PCSF Subnetwork"
    )
  })

  build_single_module_export_data <- function(module_id) {
    if (rv$module_status != "finished") return(NULL)
    if (is.null(rv$module_res) || is.null(rv$module_res$module_table) || is.null(rv$mrn)) return(NULL)

    tbl <- rv$module_res$module_table %>%
      dplyr::filter(as.character(Module) == as.character(module_id))
    if (nrow(tbl) <= 1) return(NULL)

    g <- igraph::induced_subgraph(rv$mrn, vids = unique(as.character(tbl$INCHI_ID)))
    if (!igraph::is.igraph(g) || igraph::vcount(g) == 0) return(NULL)

    node_id <- as.character(igraph::V(g)$name)
    type_map <- stats::setNames(as.character(tbl$Type), as.character(tbl$INCHI_ID))
    node_group <- ifelse(type_map[node_id] == "annotated", "Annotated", "Hidden")

    nodes <- data.frame(
      id = node_id,
      label = ifelse(
        !is.na(tbl$Compound_name[match(node_id, tbl$INCHI_ID)]) &
          nzchar(tbl$Compound_name[match(node_id, tbl$INCHI_ID)]),
        tbl$Compound_name[match(node_id, tbl$INCHI_ID)],
        node_id
      ),
      title = ifelse(
        !is.na(tbl$Compound_name[match(node_id, tbl$INCHI_ID)]) &
          nzchar(tbl$Compound_name[match(node_id, tbl$INCHI_ID)]),
        tbl$Compound_name[match(node_id, tbl$INCHI_ID)],
        node_id
      ),
      color = ifelse(node_group == "Annotated", "#2a9d8f", "#b56576"),
      stringsAsFactors = FALSE
    )

    edges_df <- igraph::as_data_frame(g, what = "edges")
    edges <- data.frame(
      from = as.character(edges_df$from),
      to = as.character(edges_df$to),
      color = "rgba(120,120,120,0.5)",
      stringsAsFactors = FALSE
    )

    list(
      g = g,
      nodes = nodes,
      edges = edges,
      node_group = stats::setNames(node_group, node_id),
      edge_width = rep(1.2, igraph::ecount(g)),
      legend_labels = c("Annotated", "Hidden"),
      title = paste0("Module ", module_id, " Network"),
      module_id = module_id
    )
  }

  module_export_info <- reactive({
    state <- selected_results_modules()
    requested <- state$requested
    if (length(requested) == 0) {
      return(list(
        mode = state$mode,
        requested = character(0),
        graphs = list(),
        missing = character(0),
        is_ready = FALSE
      ))
    }

    graphs <- lapply(requested, build_single_module_export_data)
    names(graphs) <- requested
    keep <- vapply(graphs, function(x) !is.null(x), logical(1))
    graphs <- graphs[keep]
    ready_ids <- names(graphs)
    missing <- setdiff(requested, ready_ids)

    list(
      mode = state$mode,
      requested = requested,
      graphs = graphs,
      missing = missing,
      is_ready = length(requested) > 0 && length(missing) == 0
    )
  })

  build_network_widget <- function(nodes, edges) {
    visNetwork(nodes, edges, width = "100%", height = "760px") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE)
  }

  output$results_overview <- renderUI({
    payload <- export_payload()
    module_table <- payload$module_table
    annotation_table2 <- payload$anno2

    n_uploaded <- if (is.data.frame(rv$feature_data)) nrow(rv$feature_data) else 0

    feature_col_table2 <- if (is.data.frame(annotation_table2) && nrow(annotation_table2) > 0) {
      pick_first_col(annotation_table2, c("variable_id", "feature_id", "feature", "feature_name", "id"))
    } else {
      NULL
    }

    n_annotated <- if (is.data.frame(annotation_table2) && nrow(annotation_table2) > 0) {
      if (!is.null(feature_col_table2)) {
        length(unique(stats::na.omit(annotation_table2[[feature_col_table2]])))
      } else {
        nrow(annotation_table2)
      }
    } else {
      0
    }

    module_col <- if (is.data.frame(module_table) && nrow(module_table) > 0) {
      pick_first_col(module_table, c("Module", "module", "module_id"))
    } else {
      NULL
    }

    n_modules <- if (is.data.frame(module_table) && nrow(module_table) > 0) {
      if (!is.null(module_col)) length(unique(stats::na.omit(module_table[[module_col]]))) else 0
    } else {
      0
    }

    n_export_ready <- sum(vapply(payload, is_export_ready, logical(1)))
    function_status <- if (is_export_ready(payload$module_function)) {
      "Completed"
    } else if (rv$function_status == "running") {
      "Running"
    } else if (rv$function_status == "finished") {
      "No output"
    } else {
      "Not run"
    }

    fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

    tags$div(
      class = "results-overview-grid",
      tags$div(
        class = "results-overview-card",
        tags$div(class = "results-overview-label", "Uploaded Features"),
        tags$div(class = "results-overview-value", fmt_n(n_uploaded)),
        tags$div(class = "results-overview-subtext", "Input table size")
      ),
      tags$div(
        class = "results-overview-card",
        tags$div(class = "results-overview-label", "Annotated Features"),
        tags$div(class = "results-overview-value", fmt_n(n_annotated)),
        tags$div(class = "results-overview-subtext", "From annotation table 2")
      ),
      tags$div(
        class = "results-overview-card",
        tags$div(class = "results-overview-label", "Detected Modules"),
        tags$div(class = "results-overview-value", fmt_n(n_modules)),
        tags$div(class = "results-overview-subtext", "Generated in Step 4")
      ),
      tags$div(
        class = "results-overview-card",
        tags$div(class = "results-overview-label", "Export-Ready Tables"),
        tags$div(class = "results-overview-value", paste0(n_export_ready, " / ", length(payload))),
        tags$div(
          class = "results-overview-subtext",
          paste0("Functional annotation: ", function_status)
        )
      )
    )
  })

  output$export_preview <- renderUI({
    payload <- export_payload()
    selected_tables <- if (is.null(input$export_items)) character(0) else input$export_items
    selected_networks <- if (is.null(input$network_export_items)) character(0) else input$network_export_items
    network_format_label <- switch(
      input$network_export_format,
      html = "HTML",
      graphml = "GraphML",
      "HTML"
    )
    module_info <- module_export_info()
    pcsf_ready <- !is.null(pcsf_export_data())
    module_ready <- isTRUE(module_info$is_ready)

    table_items <- if (length(selected_tables) > 0) {
      lapply(selected_tables, function(item) {
        item_label <- unname(export_labels[[item]])
        if (is.null(item_label) || is.na(item_label)) item_label <- item
        ready <- is_export_ready(payload[[item]])

        tags$li(
          class = "results-preview-item",
          tags$span(class = "results-preview-item-label", item_label),
          tags$span(
            class = paste("results-preview-status", if (ready) "ready" else "pending"),
            if (ready) "Ready" else "Pending"
          )
        )
      })
    } else {
      list()
    }

    network_label_map <- c(
      pcsf = paste0("PCSF subnetwork (", network_format_label, ")"),
      module = if (length(module_info$requested) == 0) {
        paste0("Module network (none selected, ", network_format_label, ")")
      } else if (identical(module_info$mode, "all")) {
        paste0("Module network (all ", length(module_info$requested), " modules, ", network_format_label, ")")
      } else if (length(module_info$requested) <= 3) {
        paste0("Module network (", paste(module_info$requested, collapse = ", "), ", ", network_format_label, ")")
      } else {
        paste0(
          "Module network (",
          paste(utils::head(module_info$requested, 3), collapse = ", "),
          " +", length(module_info$requested) - 3, " more, ",
          network_format_label, ")"
        )
      }
    )
    network_ready_map <- c(
      pcsf = pcsf_ready,
      module = module_ready
    )
    network_items <- if (length(selected_networks) > 0) {
      lapply(selected_networks, function(key) {
        item_label <- unname(network_label_map[[key]])
        if (is.null(item_label) || is.na(item_label)) item_label <- key
        ready <- isTRUE(network_ready_map[[key]])

        tags$li(
          class = "results-preview-item",
          tags$span(class = "results-preview-item-label", item_label),
          tags$span(
            class = paste("results-preview-status", if (ready) "ready" else "pending"),
            if (ready) "Ready" else "Pending"
          )
        )
      })
    } else {
      list()
    }

    preview_items <- c(table_items, network_items)
    if (length(preview_items) == 0) {
      return(
        tags$p(
          class = "results-empty-preview",
          icon("circle-info"),
          " No content selected yet."
        )
      )
    }

    tags$ul(
      class = "results-preview-list",
      preview_items
    )
  })
  
  observe({
    selected <- input$export_items
    selected_networks <- input$network_export_items
    payload <- export_payload()
    selected <- if (is.null(selected)) character(0) else selected
    selected_networks <- if (is.null(selected_networks)) character(0) else selected_networks

    table_ready <- if (length(selected) == 0) {
      TRUE
    } else {
      all(vapply(selected, function(item) is_export_ready(payload[[item]]), logical(1)))
    }

    network_ready <- if (length(selected_networks) == 0) {
      TRUE
    } else {
      all(vapply(
        selected_networks,
        function(key) {
          if (identical(key, "pcsf")) return(!is.null(pcsf_export_data()))
          if (identical(key, "module")) return(isTRUE(module_export_info()$is_ready))
          FALSE
        },
        logical(1)
      ))
    }

    has_any_selection <- (length(selected) + length(selected_networks)) > 0
    ready_for_download <- has_any_selection && table_ready && network_ready

    shinyjs::toggleState(
      "download_results",
      condition = ready_for_download
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("featuremap_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      selected <- if (is.null(input$export_items)) character(0) else input$export_items
      selected_networks <- if (is.null(input$network_export_items)) character(0) else input$network_export_items
      if ((length(selected) + length(selected_networks)) == 0) {
        stop("No content selected. Please select tables and/or networks to export.")
      }

      payload <- export_payload()
      missing_items <- selected[!vapply(selected, function(item) is_export_ready(payload[[item]]), logical(1))]
      if (length(missing_items) > 0) {
        missing_labels <- unname(export_labels[missing_items])
        missing_labels[is.na(missing_labels)] <- missing_items[is.na(missing_labels)]
        stop(
          paste0(
            "Selected table(s) are not ready: ",
            paste(missing_labels, collapse = ", ")
          )
        )
      }

      # ---- collect tables ----
      export_list <- payload[selected]
      
      # ---- temp dir ----
      tmpdir <- tempfile("featuremap_export_")
      dir.create(tmpdir)
      
      # ---- write files ----
      if (length(export_list) > 0 && input$export_format == "xlsx") {
        for (nm in names(export_list)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, nm)
          openxlsx::writeData(wb, nm, export_list[[nm]])
          openxlsx::saveWorkbook(
            wb,
            file = file.path(tmpdir, paste0(nm, ".xlsx")),
            overwrite = TRUE
          )
        }
      } else if (length(export_list) > 0 && input$export_format == "csv") {
        for (nm in names(export_list)) {
          write.csv(
            export_list[[nm]],
            file = file.path(tmpdir, paste0(nm, ".csv")),
            row.names = FALSE
          )
        }
      } else if (length(export_list) > 0 && input$export_format == "rdata") {
        save(
          list  = names(export_list),
          file  = file.path(tmpdir, "results.RData"),
          envir = list2env(export_list)
        )
      }

      graph_format <- if (is.null(input$network_export_format)) "html" else input$network_export_format
      module_info <- module_export_info()
      network_ready_map <- c(
        pcsf = !is.null(pcsf_export_data()),
        module = isTRUE(module_info$is_ready)
      )
      selected_network_ready <- unname(network_ready_map[selected_networks])
      if (length(selected_network_ready) == 0) selected_network_ready <- logical(0)
      selected_network_ready[is.na(selected_network_ready)] <- FALSE
      missing_networks <- selected_networks[!selected_network_ready]
      if (length(missing_networks) > 0) {
        missing_network_labels <- c(
          pcsf = "PCSF subnetwork",
          module = "Module network"
        )
        module_detail <- if ("module" %in% missing_networks && length(module_info$missing) > 0) {
          paste0(" (missing module(s): ", paste(module_info$missing, collapse = ", "), ")")
        } else {
          ""
        }
        stop(
          paste0(
            "Selected network(s) are not ready: ",
            paste(unname(missing_network_labels[missing_networks]), collapse = ", "),
            module_detail
          )
        )
      }

      if ("pcsf" %in% selected_networks) {
        pcsf_obj <- pcsf_export_data()
        if (identical(graph_format, "html")) {
          htmlwidgets::saveWidget(
            build_network_widget(pcsf_obj$nodes, pcsf_obj$edges),
            file = file.path(tmpdir, "pcsf_network.html"),
            selfcontained = TRUE
          )
        } else if (identical(graph_format, "graphml")) {
          igraph::write_graph(
            pcsf_obj$g,
            file = file.path(tmpdir, "pcsf_network.graphml"),
            format = "graphml"
          )
        } else {
          stop("Unsupported network format: ", graph_format)
        }
      }

      if ("module" %in% selected_networks) {
        module_graphs <- module_info$graphs
        for (module_obj in module_graphs) {
          module_tag <- gsub("[^A-Za-z0-9._-]+", "_", as.character(module_obj$module_id))
          if (identical(graph_format, "html")) {
            htmlwidgets::saveWidget(
              build_network_widget(module_obj$nodes, module_obj$edges),
              file = file.path(tmpdir, paste0("module_network_", module_tag, ".html")),
              selfcontained = TRUE
            )
          } else if (identical(graph_format, "graphml")) {
            igraph::write_graph(
              module_obj$g,
              file = file.path(tmpdir, paste0("module_network_", module_tag, ".graphml")),
              format = "graphml"
            )
          } else {
            stop("Unsupported network format: ", graph_format)
          }
        }
      }

      files_to_zip <- list.files(tmpdir)
      if (length(files_to_zip) == 0) {
        stop("No exportable content is available yet. Run PCSF/module steps or select ready tables.")
      }
      
      # ---- zip everything ----
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(tmpdir)
      
      zip(
        zipfile = file,
        files   = files_to_zip
      )
      
    }
    
  )
  
  output$analysis_parameters <- renderUI({
    if (is.null(rv$feature_data)) {
      return(
        tags$p(
          style = "color:#6c757d;",
          "No analysis has been run yet. Parameters will appear here after data upload."
        )
      )
    }
    
    tags$div(
      
      ## -------- Step 1 --------
      tags$h5("Step 1 – Data & Reference"),
      tags$ul(
        tags$li(paste("Reference database:", rv$ref_database_type)),
        tags$li(paste("Feature table:", nrow(rv$feature_data), "features")),
        tags$li(
          if (is.null(rv$ms2_data)) "MS2 table: Not provided"
          else "MS2 table: Provided"
        )
      ),
      
      ## -------- Step 2 --------
      tags$h5("Step 2 – Feature Annotation"),
      if (rv$annotation_status != "finished") {
        tags$p("Feature annotation: Not run yet.")
      } else {
        tags$ul(
          tags$li(paste("Column type:", toupper(input$column_type))),
          tags$li(paste("MS1 tolerance:", input$ms1_ppm, "ppm")),
          tags$li(paste("RT tolerance:", input$rt_tol, "s")),
          tags$li(paste("Isotope number:", input$isotope_num)),
          tags$li(paste("Threads:", input$workers))
        )
      },
      
      
      ## -------- Step 3 --------
      tags$h5("Step 3 – Subnetwork (PCSF)"),
      if (rv$pcsf_status != "finished") {
        tags$p("Subnetwork extraction: Not run yet")
      } else {
        tags$ul(
          tags$li(paste("Features as terminals:", input$pcsf_feature)),
          if (input$pcsf_feature == "sig")
            tags$li(paste("Significance cutoff:", input$pcsf_pcutoff)),
          tags$li(
            if (input$pcsf_grid_tune) "PCSF mode: Grid tuning"
            else "PCSF mode: Fixed parameters"
          ),
          if (!input$pcsf_grid_tune)
            tags$li(paste("b =", input$pcsf_b, ", w =", input$pcsf_w)),
          if (input$pcsf_grid_tune && !is.null(rv$annotation_object$tuning_best))
            tags$li(
              paste(
                "Best parameters:",
                "b =", rv$annotation_object$tuning_best$b,
                ", w =", rv$annotation_object$tuning_best$w
              )
            )
        )
      },
      
      ## -------- Step 4 --------
      tags$h5("Step 4 – Module Identification"),
      
      if (rv$module_status != "finished") {
        
        tags$p("Module identification: Not run yet.")
        
      } else {
        
        tags$ul(
          tags$li(paste("Algorithm:", input$module_method)),
          tags$li(paste("Node frequency threshold:", input$node_freq)),
          tags$li(
            paste(
              "Module size:",
              input$min_module_size, "–", input$max_module_size
            )
          ),
          tags$li(paste("Permutations:", input$n_perm)),
          tags$li(paste("Random seed:", input$module_seed))
        )
      },
      
      
      ## -------- Step 5 --------
      tags$h5("Step 5 – Functional Annotation"),
      if (is.null(rv$function_result)) {
        tags$p("Functional annotation: Skipped")
      } else {
        tags$ul(
          tags$li(
            paste(
              "Significance cutoff:",
              input$func_p_type, "<", input$func_p_cutoff
            )
          ),
          tags$li(paste("LLM model:", input$func_llm_model)),
          tags$li(
            if (input$func_use_pubmed) "PubMed literature: Enabled"
            else "PubMed literature: Disabled"
          )
        )
      }
    )
  })
  
  

}

# Run the application===========================================================
shinyApp(ui = ui, server = server)

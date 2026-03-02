library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(plotly)
library(visNetwork)
library(ggplot2)
library(bslib)
library(tidyverse)
library(R6)
library(future)
library(furrr)
library(igraph)
library(tidygraph)
library(purrr)
library(glue)
library(jsonlite)
library(metid)
library(PCSF2)

# setwd("~/Documents/AtNTUlife/ShenLab/1_MyProject/featureFMA/shiny")
# source("R/10_annotate_feature.R")
# source("R/11_annotate_feature_table.R")
# source("R/12_functions_data_checking.R")
# source("R/13_functions_mfc.R")
# source("R/14_functions_redundancy.R")
# source("R/15_annotation_wrapper.R")
# source("R/20_extract_subnetwork.R")
# source("R/30_identify_module.R")
# source("R/31_cal_module_score.R")
# source("R/32_permutation_test.R")
# source("R/40_annotate_module.R")
# source("R/41_rag_pubmed.R")
# source("R/42_rag_embed.R")
# source("R/43_rag_rerank.R")
# source("R/44_tidy_input.R")
# source("R/45_gen_prompt.R")
# source("R/46_gen_report.R")

lapply(
  list.files("R", full.names = TRUE),
  source
)


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

    
    
      /* running */
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      .pulse-animation {
        animation: pulse 1.5s ease-in-out infinite;
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
      
      /* Hero Section */
      .hero-section {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 80px 20px;
        text-align: center;
        margin-bottom: 40px;
      }
      
      .hero-title {
        font-size: 48px;
        font-weight: 700;
        margin-bottom: 20px;
      }
      
      .hero-subtitle {
        font-size: 20px;
        opacity: 0.95;
        margin-bottom: 40px;
      }
      
      .hero-logo {
        width: 200px;
        height: 120px;
        margin-bottom: 30px;
      }
      
      .hero-buttons {
        display: flex;
        gap: 20px;
        justify-content: center;
        margin-top: 30px;
      }
      
      .btn-hero {
        padding: 12px 30px;
        font-size: 16px;
        border-radius: 25px;
        font-weight: 600;
        transition: all 0.3s;
      }
      
      .btn-hero-primary {
        background-color: white;
        color: #667eea;
        border: none;
      }
      
      .btn-hero-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.2);
        background-color: #f8f9fa;
        color: #667eea;
      }
      
      .btn-hero-secondary {
        background-color: transparent;
        color: white;
        border: 2px solid white;
      }
      
      .btn-hero-secondary:hover {
        background-color: white;
        color: #667eea;
        transform: translateY(-2px);
      }
      
      /* Key Features Section */
      .section-title {
        text-align: center;
        font-size: 32px;
        font-weight: 700;
        color: #2c3e50;
        margin: 50px 0 30px 0;
      }
      
      .feature-card {
        background: white;
        border-radius: 12px;
        padding: 30px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        transition: all 0.3s;
        height: 100%;
      }
      
      .feature-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 16px rgba(0,0,0,0.15);
      }
      
      .feature-icon {
        font-size: 48px;
        color: #667eea;
        margin-bottom: 15px;
      }
      
      .feature-number {
        font-size: 36px;
        font-weight: 700;
        color: #667eea;
        margin-bottom: 10px;
      }
      
      .feature-title {
        font-size: 18px;
        font-weight: 600;
        color: #2c3e50;
      }
      
      /* Workflow Section */
      .workflow-section {
        background-color: #f8f9fa;
        padding: 50px 20px;
        margin-top: 40px;
      }
      
      .workflow-card {
        background: white;
        border-radius: 12px;
        padding: 30px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      /* Citation Section – Journal Style */
      .citation-section {
        background-color: #f8f9fa;
        padding: 50px 20px;
        margin-top: 0px;
      }
      
      .citation-card {
        background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
        color: white;
        border-radius: 12px;
        padding: 30px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.15);
      }
      
      .citation-text {
        font-size: 16px;
        font-style: italic;
        line-height: 1.6;
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

      
      /* Content container */
      .content-wrapper {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 20px;
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
          class = "content-wrapper",
          tags$img(
            src = "pack_shiny.png",
            class = "hero-logo",
            alt = "FeatureMAP Shiny Logo"
          ),
          tags$h1(class = "hero-title", "FeatureMAP (Online Service)"),
          tags$p(class = "hero-subtitle", 
                 "A Metabolite Feature-based Functional Module identification and Annotation Pipeline"),
          tags$div(
            class = "hero-buttons",
            actionButton("get_started", "Get Started", 
                         icon = icon("play"), 
                         class = "btn btn-hero btn-hero-secondary"),
            tags$a(
              href = "https://github.com/ytwangZero",
              target = "_blank",
              class = "btn btn-hero btn-hero-secondary",
              icon("book"),
              "Documentation"
            )
          )
        )
      ),
      
      # Key Features Section
      tags$div(
        class = "content-wrapper",
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
              tags$div(class = "feature-title", "Curated Databases")
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
              tags$div(class = "feature-title", "Metabolite Nodes")
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
              tags$div(class = "feature-title", "Reaction Edges")
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
              tags$div(class = "feature-title", "Comprehensice Network")
            )
          )
        )
      ),
      
      # Workflow Section
      tags$div(
        class = "workflow-section",
        tags$div(
          class = "content-wrapper",
          tags$h2(class = "section-title", "Workflow Overview"),
          tags$div(
            class = "workflow-card",
            plotOutput("workflow_diagram", height = 400)
          )
        )
      ),
      
      # Citation Section
      tags$div(
        class = "citation-section",
        tags$div(
          class = "content-wrapper",
          tags$h2(class = "section-title", "Citation"),
          tags$div(
            class = "citation-card",
            tags$div(
              class = "citation-text",
              tags$p(style = "font-size: 18px; margin-bottom: 10px;",
                     "If you use FeatureMAP in your research, please cite:"),
              tags$p(style = "margin-left: 20px;",
                     tags$strong("FeatureMAP: "), 
                     "A Comprehensive Pipeline for Metabolite Feature-based ",
                     "Functional Module Identification and Annotation. ",
                     tags$em("Journal Name"), ", Year;Volume:Pages.")
            ),
            actionButton("copy_citation", "Copy Citation", 
                         icon = icon("copy"),
                         class = "btn btn-light")
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
          class = "step-box",
          tags$div(class = "step-number", "Step 1"),
          tags$div(class = "step-title", "Data Upload"),
          tags$div(class = "step-description", "Upload your metabolomics feature table and reference databases")
        ),
        # ================= Step 1: Data Upload =================
        fluidRow(
          
          # ================= LEFT: Reference Databases =================
          column(
            width = 6,
            tags$div(
              class = "card",
              tags$div(class = "card-header", "Reference Databases"),
              tags$div(
                class = "card-body",
                
                # Compound Annotation Database（从 Feature Table 移过来）
                selectInput(
                  "ref_db",
                  "Compound Annotation Database",
                  choices = c("HMDB" = "hmdb", "KEGG" = "kegg")
                ),
                
                tags$hr(),
                
                conditionalPanel(
                  condition = "input.ref_db == 'hmdb'",
                  fileInput(
                    "hmdb_file",
                    "HMDB Database (.rda)",
                    accept = c(".rda", ".RData")
                  )
                ),
                
                conditionalPanel(
                  condition = "input.ref_db == 'kegg'",
                  fileInput(
                    "kegg_file",
                    "KEGG Database (.rda)",
                    accept = c(".rda", ".RData")
                  )
                ),
                
                fileInput(
                  "mrn_file",
                  "Metabolic Reaction Network (.rda)",
                  accept = c(".rda", ".RData")
                ),
                
                tags$hr(),
                
                tags$div(
                  actionButton(
                    "load_refs",
                    "Load References",
                    icon = icon("upload"),
                    class = "btn-primary"
                  ),
                  actionButton(
                    "use_default_refs",
                    "Use Default References",
                    icon = icon("upload"),
                    class = "btn-outline-secondary"
                  )
                )
              )
            )
          ),
          
          # ================= RIGHT: Feature + MS2 Tables =================
          column(
            width = 6,
            tags$div(
              class = "card",
              tags$div(class = "card-header", "Data Tables"),
              tags$div(
                class = "card-body",
                
                fileInput(
                  "feature_file",
                  "Feature Table (.rda, .csv)",
                  accept = c(".rda", ".csv", ".Rdata")
                ),
                
                tags$small(
                  style = "color:#6c757d;",
                  "Required columns: variable_id, mz, rt, polarity, condition, and pvalue."
                ),
                
                tags$hr(),
                
                fileInput(
                  "ms2_file",
                  "MS2 Annotation Table (.rda, .csv)",
                  accept = c(".rda", ".csv", ".Rdata")
                ),

                
                tags$small(
                  style = "color:#6c757d;",
                  "Note: MS2 annotation table is optional."
                ),
                
                tags$hr(),
                
                tags$div(
                  style = "display:flex; gap:10px;",
                  actionButton(
                    "load_feature_ms2",
                    "Load Uploaded Data",
                    icon = icon("upload"),
                    class = "btn-primary"
                  ),
                  actionButton(
                    "use_demo_both",
                    "Use Demo Data",
                    icon = icon("upload"),
                    class = "btn-outline-secondary"
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "card",
              tags$div(class = "card-header card-header-result", 
                       icon("table"),
                       "Data Preview"),
              tags$div(
                class = "card-body",
                DTOutput("data_preview")
              )
            )
          )
        ),
        tags$div(
          class = "next-button-container",
          actionButton(
            "next_to_annotation",
            "Next Step",
            icon = icon("arrow-right-from-bracket"),
            class = "btn btn-primary btn-next",
            style = "display: none;"
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
          class = "step-box",
          tags$div(class = "step-number", "Step 2"),
          tags$div(class = "step-title", "Feature Annotation"),
          tags$div(class = "step-description", "Match features to metabolite databases using MS1 or MS2 information")
        ),
        fluidRow(
          column(
            width = 4,
            tags$div(
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
                actionButton("run_annotation", "Run", 
                             icon = icon("play-circle"), 
                             class = "btn-success btn-lg w-100")
              )
            )
          ),
          column(
            width = 8,
            tags$div(
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("layer-group"),
                "Annotation Results"
              ),
              tags$div(
                class = "card-body",
                
                # ===== Status: workflow-level =====
                uiOutput("annotation_status_text"),
                tags$hr(),
                
                # ===== Tabs: content-level =====
                tabsetPanel(
                  id = "annotation_tabs",
                  type = "tabs",
                  
                  tabPanel(
                    title = tagList(icon("chart-simple"), "Summary"),
                    uiOutput("annotation_mfc_ui"),
                    tags$hr(),
                    uiOutput("annotation_feature_ui")
                  ),
                  
                  tabPanel(
                    title = tagList(icon("table"), "Annotation Table 1"),
                    DTOutput("annotated_table")
                  )
                )
              )
            )
          )
        ),
        uiOutput("next_button_annotation")
      )
    ),
    
    # Network Extraction Page
    tags$div(
      id = "network_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box",
          tags$div(class = "step-number", "Step 3"),
          tags$div(class = "step-title", "Subnetwork Extraction"),
          tags$div(class = "step-description", "Extract optimal subnetwork using Prize-Collecting Steiner Forest algorithm")
        ),
        fluidRow(
          
          # ===== LEFT: Parameters =====
          column(
            width = 4,
            tags$div(
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
                  )
                ),
                
                tags$div(
                  class = "param-card",
                  tags$h4("PCSF Parameters"),
                  
                  checkboxInput(
                    "pcsf_grid_tune",
                    "Enable grid tuning (b & w)",
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
                
                actionButton(
                  "run_pcsf",
                  "Run PCSF",
                  icon = icon("play-circle"),
                  class = "btn-success btn-lg w-100"
                )
              )
            )
          ),
          
          # ===== RIGHT: Status + Network =====
          column(
            width = 8,
            tags$div(
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("chart-line"),
                "PCSF Subnetwork"
              ),
              tags$div(
                class = "card-body",
                
                # ===== Status（workflow-level）=====
                uiOutput("pcsf_status_text"),
                tags$hr(),
                
                # ===== Tabs（content-level）=====
                tabsetPanel(
                  id = "pcsf_tabs",
                  type = "tabs",
                  
                  # ---- Tab 1: Subnetwork ----
                  tabPanel(
                    title = tagList(icon("network-wired"), "Subnetwork"),
                    uiOutput("pcsf_network_container"),
                    uiOutput("pcsf_tuning_summary")
                  ),
                  
                  # ---- Tab 2: Annotation Table 2 ----
                  tabPanel(
                    title = tagList(icon("table"), "Annotation Table 2"),
                    DTOutput("annotation_table2")
                  )
                )
              )
            )
          )
        ),
        uiOutput("next_button_network")
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
          class = "step-box",
          tags$div(class = "step-number", "Step 4"),
          tags$div(class = "step-title", "Module Identification"),
          tags$div(
            class = "step-description",
            "Identify functional modules using community detection on the PCSF subnetwork"
          )
        ),
        
        # ================= Main panel =================
        fluidRow(
          
          # ========== LEFT: Parameters ==========
          column(
            width = 4,
            tags$div(
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
                
                actionButton(
                  "run_modules",
                  "Run",
                  icon = icon("play-circle"),
                  class = "btn-success btn-lg w-100"
                )
              )
            )
          ),
          
          # ================= RIGHT: Outputs =================
          column(
            width = 8,
            tags$div(
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("layer-group"),
                "Module Results"
              ),
              tags$div(
                class = "card-body",
                
                # ===== Status（workflow-level）=====
                uiOutput("module_status_text"),
                tags$hr(),
                
                # ===== Tabs（content-level）=====
                tabsetPanel(
                  id = "module_tabs",
                  type = "tabs",
                  
                  # ---- Tab 1: Visualization ----
                  tabPanel(
                    title = tagList(icon("network-wired"), "Visualization"),
                    uiOutput("module_selector_ui"),
                    tags$hr(),
                    uiOutput("module_network_ui")
                  ),
                  
                  # ---- Tab 2: Module Score ----
                  tabPanel(
                    title = tagList(icon("chart-line"), "Module Score"),
                    DTOutput("module_score")
                  ),
                  
                  # ---- Tab 3: Module Table ----
                  tabPanel(
                    title = tagList(icon("table"), "Module Table"),
                    DTOutput("module_table")
                  )
                )
              )
            )
          )
        ),
        
        # ================= Next =================
        uiOutput("next_button_modules")
      )
    ),
    
    # Function Annotation Page
    tags$div(
      id = "function_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box",
          tags$div(class = "step-number", "Step 5"),
          tags$div(class = "step-title", "AI-Powered Function Annotation"),
          tags$div(class = "step-description", "Automatically annotate module functions using large language models")
        ),
        
        fluidRow(
          
          # ===== LEFT: Parameters =====
          column(
            width = 4,
            tags$div(
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
                    
                    sliderInput(
                      "func_top_compound",
                      "Top metabolites (%)",
                      min = 5, max = 50, value = 10, step = 5
                    ),
                    
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
                    ),
                    
                    textInput(
                      "func_embed_model",
                      "Embedding model",
                      value = "text-embedding-3-small",
                      placeholder = "e.g. text-embedding-3-small"
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
                  
                  textInput(
                    "func_llm_model",
                    "LLM model",
                    placeholder = "e.g. gpt-4.1-mini"
                  ),
                  
                  
                  passwordInput(
                    "func_llm_key",
                    "LLM API Key"
                  )
                ),
                
                tags$hr(),
                
                actionButton(
                  "run_function_annotation",
                  "Run",
                  icon = icon("play-circle"),
                  class = "btn-success btn-lg w-100"
                )
              )
            )
          ),
          
          
          # ===== RIGHT: Outputs =====
          column(
            width = 8,
            
            tags$div(
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("robot"),
                "Module Functional Annotation"
              ),
              tags$div(
                class = "card-body",
                uiOutput("function_status_text"),
                tags$hr(),
                uiOutput("function_main_ui")
              )
              
            )
          )
        ),
        
        tags$div(
          class = "next-button-container",
          actionButton(
            "next_to_results",
            "Go to Results",
            icon = icon("download"),
            class = "btn btn-primary btn-next"
          )
        )
      )
    ),
    
    # Results Export Page
    tags$div(
      id = "results_page",
      style = "display: none;",
      tags$div(
        class = "content-wrapper",
        tags$div(
          class = "step-box",
          tags$div(class = "step-number", "Congratulations!"),
          tags$div(class = "step-title", "Results Export"),
          tags$div(class = "step-description", "Review analysis results and export comprehensive reports")
        ),
        fluidRow(
          
          # ===== LEFT: Export options =====
          column(
            width = 4,
            tags$div(
              class = "card",
              tags$div(class = "card-header", "Export Options"),
              tags$div(
                class = "card-body",
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
                selectInput("export_format", "Export Format",
                            choices = c("Excel (.xlsx)" = "xlsx",
                                        "CSV (multiple files)" = "csv",
                                        "RData" = "rdata")),
                tags$hr(),
                downloadButton("download_results", "Download Results",
                               class = "btn-success btn-lg w-100")
              )
            )
          ),
          # ===== RIGHT: Analysis Parameters =====
          column(
            width = 8,
            tags$div(
              class = "card",
              tags$div(
                class = "card-header card-header-result",
                icon("sliders"),
                "Analysis Parameters"
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
    ### Step 4
    module_res    = NULL,
    module_status = "idle",
    ### Step 5
    function_result = NULL,
    function_status = "idle"
  )
  
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
    
    rv$current_page <- page
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
  
  # Get started button
  observeEvent(input$get_started, {
    switch_page("upload")
  })
  
  # Copy citation button
  observeEvent(input$copy_citation, {
    showNotification("Citation copied to clipboard!", type = "message")
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
  observeEvent(input$load_feature_ms2, {
    
    # -------- Feature Table --------
    if (!is.null(input$feature_file)) {
      
      file_ext <- tools::file_ext(input$feature_file$name)
      
      dat <- tryCatch({
        if (file_ext == "csv") {
          read.csv(input$feature_file$datapath, check.names = FALSE)
        } else {
          e <- new.env()
          load(input$feature_file$datapath, envir = e)
          e[[ls(e)[1]]]
        }
      }, error = function(e) NULL)
      
      if (!is.null(dat)) {
        rv$feature_data <- dat
        showNotification("Feature table loaded.", type = "message")
      }
    }
    
    # -------- MS2 Table（optional） --------
    if (!is.null(input$ms2_file)) {
      
      file_ext <- tools::file_ext(input$ms2_file$name)
      
      ms2 <- tryCatch({
        if (file_ext == "csv") {
          read.csv(input$ms2_file$datapath, check.names = FALSE)
        } else {
          e <- new.env()
          load(input$ms2_file$datapath, envir = e)
          e[[ls(e)[1]]]
        }
      }, error = function(e) {
        showNotification(
          paste("Failed to load MS2 table:", e$message),
          type = "error",
          duration = NULL
        )
        NULL
      })
      
      if (!is.null(ms2)) {
        rv$ms2_data <- ms2
        showNotification("MS2 table loaded.", type = "message")
      }
    }
    
    
    shinyjs::show("next_to_annotation")
  })
  
  
  observeEvent(input$use_demo_both, {
    
    # Demo Feature Table
    e1 <- new.env()
    load("www/demo_feature_table.rda", envir = e1)
    rv$feature_data <- e1[[ls(e1)[1]]]
    
    # Demo MS2 Table
    ms2_file <- if (input$ref_db == "hmdb") {
      "www/demo_ms2_hmdb.rda"
    } else {
      "www/demo_ms2_kegg.rda"
    }
    
    e2 <- new.env()
    load(ms2_file, envir = e2)
    rv$ms2_data <- e2[[ls(e2)[1]]]
    
    rv$using_demo_features <- TRUE
    
    showNotification(
      paste(
        "Demo feature table loaded.",
        "Demo MS2 table (", toupper(input$ref_db), ") loaded."
      ),
      type = "message"
    )
    
    shinyjs::show("next_to_annotation")
  })
  
  
  
  # Placeholder outputs
  output$data_preview <- renderDT({
    if (is.null(rv$feature_data)) {
      return(
        datatable(
          data.frame(Message = "No data loaded yet"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }
    datatable(
      rv$feature_data,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  observeEvent(input$load_refs, {
    # 1. MRN is mandatory
    req(input$mrn_file)
    
    # Load MRN
    e_mrn <- new.env()
    load(input$mrn_file$datapath, envir = e_mrn)
    rv$mrn <- e_mrn[[ls(e_mrn)[1]]]
    
    # 2. Load annotation database
    req(input$ref_db)
    
    if (input$ref_db == "hmdb") {
      req(input$hmdb_file)
      e <- new.env()
      load(input$hmdb_file$datapath, envir = e)
      rv$ref_database <- e[[ls(e)[1]]]
      rv$ref_database_type <- "HMDB"
      
    } else if (input$ref_db == "kegg") {
      req(input$kegg_file)
      e <- new.env()
      load(input$kegg_file$datapath, envir = e)
      rv$ref_database <- e[[ls(e)[1]]]
      rv$ref_database_type <- "KEGG"
    }
    
    showNotification(
      paste(
        "References loaded successfully."
      ),
      type = "message"
    )
  })
  
  observeEvent(input$use_default_refs, {
    req(input$ref_db)
    # Load default MRN
    e_mrn <- new.env()
    load("www/metabolic_reaction_network.rda", envir = e_mrn)
    rv$mrn <- e_mrn[[ls(e_mrn)[1]]]
    # Load selected default annotation database
    if (input$ref_db == "hmdb") {
      e <- new.env()
      load("www/hmdb_compound_ms1.rda", envir = e)
      rv$ref_database <- e[[ls(e)[1]]]
      rv$ref_database_type <- "HMDB (default)"
    } else if (input$ref_db == "kegg") {
      e <- new.env()
      load("www/kegg_compound_ms1.rda", envir = e)
      rv$ref_database <- e[[ls(e)[1]]]
      rv$ref_database_type <- "KEGG (default)"
    }
    rv$using_default_refs <- TRUE
    showNotification(
      paste(
        "Default reference data loaded!"
      ),
      type = "message"
    )
  })
  
  observeEvent(input$next_to_annotation, {
    switch_page("annotation")
  })
  
  ## Step 2 Feature annotation====================================
  observeEvent(input$run_annotation, {
    
    req(rv$feature_data, rv$ref_database)
    
    rv$annotation_status <- "running"
    shinyjs::disable("run_annotation")
    shinyjs::delay(100, {
      res <- tryCatch({
        annotate_feature(
          feature_table = rv$feature_data,
          ms2_table = rv$ms2_data,
          metabolite_database = rv$ref_database,
          refdata = input$ref_db,
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
        shinyjs::enable("run_annotation")
      } else {
        rv$annotation_object  <- res
        rv$annotated_table    <- res$anno_feature_table1
        rv$annotation_summary <- res$anno_redundancy
        rv$annotation_status  <- "finished"
        
        shinyjs::enable("run_annotation")
      }
      
      shinyjs::enable("run_annotation")
    })
  })

  output$annotation_status_text <- renderUI({
    if (rv$annotation_status == "running") {
      tags$h4(
        class = "pulse-animation",
        icon("spinner", class = "fa-spin"),
        " Running feature annotation, please wait ..."
      )
    } else if (rv$annotation_status == "finished") {
      tags$h4(
        style = "color: #28a745;",
        icon("check-circle"),
        " Step 2 completed successfully!"
      )
    } else {
      tags$h4("Status: Ready")
    }
  })

  output$next_button_annotation <- renderUI({
    req(rv$annotated_table)
    tags$div(
      class = "next-button-container",
      actionButton(
        "next_to_network",
        "Next Step",
        icon = icon("arrow-right-from-bracket"),
        class = "btn btn-primary btn-next"
      )
    )
  })

  observeEvent(input$next_to_network, {
    switch_page("network")
  })
  
  output$annotation_mfc_ui <- renderUI({
    if (rv$annotation_status == "running") {
      tags$div(
        class = "empty-panel",
        icon("spinner", class = "fa-spin"),
        "Summarizing annotation results ..."
      )
    } else if (rv$annotation_status == "finished") {
      plotlyOutput("annotation_mfc_plot", height = 280)
    } else {
      tags$div(
        class = "empty-panel",
        icon("chart-bar"),
        "No annotation results yet."
      )
    }
  })
  
  output$annotation_feature_ui <- renderUI({
    if (rv$annotation_status == "running") {
      NULL
    } else if (rv$annotation_status == "finished") {
      plotlyOutput("annotation_feature_plot", height = 280)
    } else {
      NULL

    }
  })
  
  output$annotation_mfc_plot <- renderPlotly({
    req(rv$annotation_summary)
    
    df <- rv$annotation_summary %>%
      tibble::rownames_to_column("stage") %>%
      mutate(
        stage = factor(stage, levels = c("before", "after")),
        color = ifelse(stage == "after", "#3d5c6f", "#92bbd7")
      )
    
    ymax <- max(df$mfc_per_metabolite) * 1.15
    
    plot_ly(
      df,
      x = ~mfc_per_metabolite,
      y = ~stage,
      type = "bar",
      orientation = "h",
      marker = list(color = ~color),
      text = ~round(mfc_per_metabolite, 3),
      textposition = "outside"
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "MFC per metabolite"),
        yaxis = list(title = "")
      )
    
  })
  
  output$annotation_feature_plot <- renderPlotly({
    req(rv$annotation_summary)
    
    df <- rv$annotation_summary %>%
      tibble::rownames_to_column("stage") %>%
      mutate(
        stage = factor(stage, levels = c("before", "after")),
        color = ifelse(stage == "after", "#e47159", "#f3c1ad")
      )
    
    ymax <- max(df$metabolite_per_feature) * 1.15
    
    plot_ly(
      df,
      x = ~metabolite_per_feature,
      y = ~stage,
      type = "bar",
      orientation = "h",
      marker = list(color = ~color),
      text = ~round(metabolite_per_feature, 2),
      textposition = "outside"
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Metabolites per feature"),
        yaxis = list(title = "")
      )
    
  })
  
  output$annotated_table <- renderDT({
    req(rv$annotated_table)
    datatable(
      rv$annotated_table,
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  ## Step 3: Extract Subnetwork ======================================
  output$pcsf_status_text <- renderUI({
    if (rv$pcsf_status == "running") {
      tags$h4(
        class = "pulse-animation",
        icon("spinner", class = "fa-spin"),
        " Running subnetwork extraction, please wait ..."
      )
    } else if (rv$pcsf_status == "finished") {
      tags$h4(
        style = "color: #28a745;",
        icon("check-circle"),
        " Step 3 completed successfully!"
      )
    } else {
      tags$h4("Status: Ready")
    }
  })
  
  observeEvent(input$run_pcsf, {
    
    req(rv$annotation_object, rv$mrn)
    
    rv$pcsf_status <- "running"
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
    
    
    shinyjs::delay(100, {
      
      res <- tryCatch({
        extract_subnetwork(
          res = rv$annotation_object,
          feature = input$pcsf_feature,
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
      } else {
        rv$annotation_object <- res
        rv$annotation_table2 <- res$anno_feature_table2
        rv$pcsf_status <- "finished"
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
    if (rv$pcsf_status == "running") {
      tags$div(
        class = "empty-panel",
        icon("spinner", class = "fa-spin"),
        "Running PCSF subnetwork extraction..."
      )
    } else if (rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network)) {
      visNetworkOutput("pcsf_network_plot", height = 660)
    } else if (rv$pcsf_status == "finished") {
      tags$div(
        class = "empty-panel",
        icon("eye-slash"),
        "Network visualization is disabled. Enable it in parameters."
      )
    } else {
      tags$div(
        class = "empty-panel",
        icon("network-wired"),
        "Subnetwork has not been generated yet."
      )
    }
  })
  
  output$pcsf_network_plot <- renderVisNetwork({
    req(rv$pcsf_status == "finished")
    req(isTRUE(input$pcsf_show_network))
    req(rv$annotation_object$pcsf_subnet)
    
    g <- rv$annotation_object$pcsf_subnet
    
    # --- nodes ---
    node_id <- as.character(igraph::V(g)$name)
    
    short_label <- ifelse(
      nchar(node_id) > 28,
      paste0(substr(node_id, 1, 25), "..."),
      node_id
    )
    
    node_prize <- igraph::V(g)$prize
    if (is.null(node_prize) || all(is.na(node_prize))) node_prize <- rep(1, length(node_id))
    node_prize <- as.numeric(node_prize)
    
    node_type <- igraph::V(g)$type
    if (is.null(node_type) || all(is.na(node_type))) node_type <- rep("Node", length(node_id))
    node_type <- as.character(node_type)
    
    nodes <- data.frame(
      id    = node_id,
      label = short_label,
      title = node_id,          
      value = scales::rescale(node_prize, to = c(10, 40)),       
      color = ifelse(
        node_type == "Terminal",
        "#fc8d58",  
        "#4474b3"    
      ),
      color.border = ifelse(
        node_type == "Terminal",
        "#fc8d58",
        "#4474b3"
      ),
      stringsAsFactors = FALSE
    )
    ledges <- data.frame(
      color = c("#fc8d58", "#4474b3"),
      label = c("Feature", "Metabolite"),
      shape = c("dot", "dot") 
    )
    
    # --- edges ---
    edges_df <- igraph::as_data_frame(g, what = "edges")
    edges_df$from <- as.character(edges_df$from)
    edges_df$to   <- as.character(edges_df$to)

    edge_freq <- igraph::E(g)$freq
    if (is.null(edge_freq) || all(is.na(edge_freq))) edge_freq <- rep(1, nrow(edges_df))
    edge_freq <- as.numeric(edge_freq)
    
    edges <- data.frame(
      from  = edges_df$from,
      to    = edges_df$to,
      value = scales::rescale(edge_freq, to = c(1, 6)),              
      title = paste0("freq: ", round(edge_freq, 3)),
      color = "rgba(120,120,120,0.5)",
      stringsAsFactors = FALSE
    )
    
    visNetwork(nodes, edges, width = "100%", height = "500px") %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visLegend(useGroups = FALSE, addNodes = ledges) %>%
      visPhysics(stabilization = FALSE)
  })
  
  output$annotation_table2 <- renderDT({
    req(rv$annotation_table2)
    
    datatable(
      rv$annotation_table2,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$next_button_network <- renderUI({
    req(rv$pcsf_status == "finished")
    
    tags$div(
      class = "next-button-container",
      actionButton(
        "next_to_modules",
        "Next Step",
        icon = icon("arrow-right-from-bracket"),
        class = "btn btn-primary btn-next"
      )
    )
  })
  
  observeEvent(input$next_to_modules, {
    switch_page("modules")
  })
  
 
  ## Step 4: Module Identification =====================================
  observeEvent(input$run_modules, {
    req(rv$annotation_object)
    req(rv$pcsf_status == "finished")
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
      } else {
        rv$module_res    <- res
        rv$mrn           <- res$mrn
        rv$module_status <- "finished"
      }
      
      
      shinyjs::enable("run_modules")
    })
  })
  output$module_network_ui <- renderUI({
    if (rv$module_status == "running") {
      tags$div(
        class = "empty-panel",
        icon("spinner", class = "fa-spin"),
        "Identifying modules..."
      )
    } else if (rv$module_status == "finished" && !is.null(input$selected_module)) {
      visNetworkOutput("module_network", height = 660)
    } else {
      tags$div(
        class = "empty-panel",
        icon("circle-info"),
        "Module has not been identified yet."
      )
    }
  })
  
  
  output$module_status_text <- renderUI({
    if (rv$module_status == "running") {
      tags$h4(
        class = "pulse-animation",
        icon("spinner", class = "fa-spin"),
        " Running module identification, please wait ..."
      )
    } else if (rv$module_status == "finished") {
      tags$h4(
        style = "color:#28a745;",
        icon("check-circle"),
        " Step 4 completed successfully!"
      )
    } else {
      tags$h4("Status: Ready")
    }
  })
  
  output$module_selector_ui <- renderUI({
    req(rv$module_res)
    req(rv$module_res$module_score)
    
    mods <- sort(unique(rv$module_res$module_score$Module))
    req(length(mods) > 0)
    
    selectInput(
      "selected_module",
      "Select module to visualize",
      choices  = mods,
      selected = mods[1]
    )
  })
  
  output$module_network <- renderVisNetwork({
    req(rv$module_res)
    req(input$selected_module)
    
    tbl <- rv$module_res$module_table %>%
      dplyr::filter(Module == input$selected_module)
    
    req(nrow(tbl) > 1)
    
    g <- induced_subgraph(
      rv$mrn,
      vids = tbl$INCHI_ID
    )
    
    node_id <- igraph::V(g)$name
    node_type <- rv$module_res$module_table %>%
      dplyr::filter(Module == input$selected_module) %>%
      dplyr::select(INCHI_ID, Type)
    
    type_map <- setNames(node_type$Type, node_type$INCHI_ID)
    
    nodes <- data.frame(
      id    = node_id,
      label = ifelse(
        nchar(node_id) > 28,
        paste0(substr(node_id, 1, 25), "..."),
        node_id
      ),
      title = node_id,
      color = ifelse(
        type_map[node_id] == "annotated",
        "#fc8d58",
        "#4474b3"
      ),
      stringsAsFactors = FALSE
    )
    
    edges_df <- igraph::as_data_frame(g, what = "edges")
    
    edges <- data.frame(
      from  = as.character(edges_df$from),
      to    = as.character(edges_df$to),
      color = "rgba(120,120,120,0.5)",
      stringsAsFactors = FALSE
    )
    
    visNetwork(nodes, edges, width = "100%", height = "500px") %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visPhysics(stabilization = FALSE) %>%
      visLegend(
        useGroups = FALSE,
        addNodes = data.frame(
          label = c("Annotated", "Hidden"),
          color = c("#fc8d58", "#4474b3"),
          shape = c("dot", "dot")
        )
      )
    
  })
  
  output$module_score <- renderDT({
    req(rv$module_res)
    req(rv$module_res$module_score)
    
    datatable(
      rv$module_res$module_score,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$module_table <- renderDT({
    req(rv$module_res)
    req(rv$module_res$module_table)
    
    datatable(
      rv$module_res$module_table,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$next_button_modules <- renderUI({
    req(rv$module_status == "finished")
    
    tags$div(
      class = "next-button-container",
      actionButton(
        "next_to_function",
        "Next Step",
        icon = icon("arrow-right-from-bracket"),
        class = "btn btn-primary btn-next"
      )
    )
  })
  
  observeEvent(input$next_to_function, {
    switch_page("function")
  })
  
  
  ## Step 5: Functional Annotation =====================================
  observeEvent(input$run_function_annotation, {
    
    req(rv$module_res)
    req(rv$module_status == "finished")
    
    # ---- basic checks ----
    req(nzchar(input$func_llm_model))
    req(nzchar(input$func_llm_key))
    
    if (isTRUE(input$func_use_pubmed)) {
      req(nzchar(input$func_embed_model))
      req(nzchar(input$func_ncbi_key))
    }
    
    ref_data <- tryCatch({
      
      if (input$func_class_source == "default") {
        
        cat("Loading default database for:", rv$module_res$refdata, "\n")
        
        e <- new.env()
        
        if (rv$module_res$refdata == "hmdb") {
          load("www/hmdb_function.Rdata", envir = e)
          cat("Loaded HMDB function database\n")
        } else {
          load("www/kegg_function.Rdata", envir = e)
          cat("Loaded KEGG function database\n")
        }
        
        result <- e[[ls(e)[1]]]
        cat("Database dimensions:", nrow(result), "x", ncol(result), "\n")
        result
        
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

    if (is.null(ref_data)) {
      cat("ref_data is NULL, aborting\n")
      return()
    }

    
    # ---- set status ----
    rv$function_status <- "running"
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
          top_compound  = input$func_top_compound / 100,
          years         = input$func_pubmed_years,
          top_paper     = input$func_top_paper,
          min_similarity= input$func_min_similarity,
          provider      = input$func_provider,
          embed_model   = input$func_embed_model,
          llm_model     = input$func_llm_model,
          NCBI_api_key  = input$func_ncbi_key,
          llm_api_key   = input$func_llm_key
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
        
        shinyjs::enable("run_function_annotation")
      }
    })
  })
  
  output$function_main_ui <- renderUI({
    
    # ---- running ----
    if (rv$function_status == "running") {
      tags$div(
        class = "empty-panel",
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
        class = "empty-panel",
        icon("triangle-exclamation"),
        "No significant modules passed the cutoff."
      )
      
      # ---- finished with result ----
    } else if (rv$function_status == "finished") {
      
      tagList(
        uiOutput("function_module_selector"),
        tags$hr(),
        uiOutput("function_module_output")
      )
      
      # ---- idle ----
    } else {
      
      tags$div(
        class = "empty-panel",
        icon("robot"),
        "Click Run to annotate module functions."
      )
    }
  })

  #### ---- Status text ----
  output$function_status_text <- renderUI({
    if (rv$function_status == "running") {
      tags$h4(
        class = "pulse-animation",
        icon("spinner", class = "fa-spin"),
        " Running functional annotation, please wait ..."
      )
    } else if (rv$function_status == "finished") {
      tags$h4(
        style = "color:#28a745;",
        icon("check-circle"),
        " Step 5 completed successfully!"
      )
    } else if (rv$function_status == "error") {
      tags$h4(
        style = "color:#dc3545;",
        icon("triangle-exclamation"),
        " Functional annotation failed."
      )
    }else {
      tags$h4("Status: Ready")
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
    
    selectInput(
      "selected_function_module",
      "Select module",
      choices  = module_ids,
      selected = module_ids[1]
    )
  })
  
  
  #### ---- Module annotation output ----
  output$function_module_output <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (is.null(rv$function_result) || is.null(rv$function_result$module_function)) return(NULL)
    if (is.null(input$selected_function_module)) return(NULL)
    
    mod_df <- rv$function_result$module_function %>%
      dplyr::filter(module_id == as.numeric(input$selected_function_module))
    
    req(nrow(mod_df) == 1)
    
    tags$div(
      
      tags$div(
        class = "param-card",
        tags$h4(icon("tag"), "Module Title"),
        tags$p(style = "font-size:18px; font-weight:600;", mod_df$title)
      ),
      
      tags$div(
        class = "param-card",
        tags$h4(icon("align-left"), "Functional Summary"),
        tags$p(style = "font-size:15px; line-height:1.6;", mod_df$summary)
      ),
      
      tags$div(
        class = "param-card",
        tags$h4(icon("chart-simple"), "Confidence"),
        tags$p(style = "font-size:15px;", mod_df$confidence_label)
      ),
      
      tags$div(
        class = "param-card",
        tags$h4(icon("think-peaks"), "Rationale"),
        tags$p(style = "font-size:14px; line-height:1.6;", mod_df$rationale)
      )
    )
  })
  
  observeEvent(input$next_to_results, {
    switch_page("results")
  })
  
  
  ## Step 6: Download the results ================================
  output$export_preview <- renderUI({
    if (is.null(input$export_items) || length(input$export_items) == 0) {
      return(tags$p("No tables selected."))
    }
    
    tags$ul(
      lapply(input$export_items, function(x) {
        tags$li(
          switch(
            x,
            anno1           = "Annotation table 1",
            anno2           = "Annotation table 2",
            module_score    = "Module score",
            module_table    = "Module table",
            module_function = "Module functional"
          )
        )
      })
    )
  })
  
  observe({
    shinyjs::toggleState(
      "download_results",
      condition = !is.null(input$export_items) && length(input$export_items) > 0
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("FeatureMAP_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(input$export_items)
      
      if ("module_function" %in% input$export_items &&
          is.null(rv$function_result)) {
        stop("Module function annotation was not generated.")
      }
      
      # ---- collect tables ----
      export_list <- list()
      for (item in input$export_items) {
        export_list[[item]] <- switch(
          item,
          anno1           = rv$annotated_table,
          anno2           = rv$annotation_table2,
          module_score    = rv$module_res$module_score,
          module_table    = rv$module_res$module_table,
          module_function = rv$function_result$module_function
        )
      }
      
      # ---- temp dir ----
      tmpdir <- tempfile("featuremap_export_")
      dir.create(tmpdir)
      
      # ---- write files ----
      if (input$export_format == "xlsx") {
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
      } else if (input$export_format == "csv") {
        for (nm in names(export_list)) {
          write.csv(
            export_list[[nm]],
            file = file.path(tmpdir, paste0(nm, ".csv")),
            row.names = FALSE
          )
        }
      } else if (input$export_format == "rdata") {
        save(
          list  = names(export_list),
          file  = file.path(tmpdir, "results.RData"),
          envir = list2env(export_list)
        )
      }
      
      # ---- zip everything ----
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(tmpdir)
      
      zip(
        zipfile = file,
        files   = list.files(".")
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


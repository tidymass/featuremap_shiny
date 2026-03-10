if (!"featuremapshiny" %in% loadedNamespaces() &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
}

shiny::runApp("inst/app")

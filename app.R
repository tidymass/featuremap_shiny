if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else if (!"featuremapshiny" %in% loadedNamespaces() &&
           !requireNamespace("featuremapshiny", quietly = TRUE)) {
  stop(
    "Install or load the featuremapshiny package, or install pkgload to run from source.",
    call. = FALSE
  )
}

shiny::runApp("inst/app")

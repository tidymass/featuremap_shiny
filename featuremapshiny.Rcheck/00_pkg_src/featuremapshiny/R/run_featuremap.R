#' Launch the FeatureMAP Shiny Application
#'
#' Starts the packaged FeatureMAP Shiny app installed under `inst/app`.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The return value from [shiny::runApp()].
#' @export
run_featuremap <- function(...) {
  app_dir <- system.file("app", package = "featuremapshiny")

  if (!nzchar(app_dir)) {
    stop("Could not find the packaged Shiny application directory.", call. = FALSE)
  }

  shiny::runApp(appDir = app_dir, ...)
}

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

  # Check before binding the port, so a public deployment with sign-in
  # switched on but not configured fails at launch, not at first sign-up.
  featuremap_check_auth_setup()

  shiny::runApp(appDir = app_dir, ...)
}

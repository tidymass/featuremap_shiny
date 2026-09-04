# Launcher for the deployed FeatureMAP Shiny app.
# Run it through run_app.sh, which sets the environment this expects.

local({
  root <- Sys.getenv("FM_ROOT", unset = path.expand("~/featuremap-deploy"))
  lib  <- Sys.getenv("FM_LIB",  unset = file.path(root, "rlib"))

  # Renviron.site on this machine hard-assigns R_LIBS_USER, so never trust the
  # environment alone: put our library first explicitly.
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

  host <- Sys.getenv("FM_HOST", unset = "0.0.0.0")
  port <- as.integer(Sys.getenv("FM_PORT", unset = "3838"))

  cat("FeatureMAP Shiny\n")
  cat("  library : ", .libPaths()[1], "\n", sep = "")
  cat("  package : ", as.character(utils::packageVersion("featuremapshiny")), "\n", sep = "")
  cat("  pandoc  : ", if (nzchar(Sys.which("pandoc"))) Sys.which("pandoc") else "NOT FOUND", "\n", sep = "")
  cat("  upload  : ", Sys.getenv("FEATUREMAP_MAX_UPLOAD_MB", "500"), " MB\n", sep = "")
  cat("  listen  : http://", host, ":", port, "\n\n", sep = "")

  # host = "0.0.0.0" is what makes the app reachable from other machines;
  # the default 127.0.0.1 would only accept connections from the server itself.
  featuremapshiny::run_featuremap(
    host = host,
    port = port,
    launch.browser = FALSE
  )
})

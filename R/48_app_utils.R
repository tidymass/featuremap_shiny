#' Zip a set of files without changing the working directory
#'
#' `utils::zip()` stores each file under the path it is given, so the common
#' workaround is `setwd(dir)` + `basename(files)`. That is unsafe inside a
#' Shiny app: all sessions share one R process, so `setwd()` from one user's
#' download handler changes the working directory for every other session that
#' happens to be running at that moment. This helper passes the `-j`
#' ("junk paths") flag to the zip backend instead, which flattens the archive
#' without any process-wide state change.
#'
#' @param zipfile Path of the archive to create.
#' @param files Character vector of file paths to place in the archive.
#'   Directories and missing paths are dropped. Paths are stored by basename
#'   only, so callers must ensure the remaining basenames are unique.
#' @param flags Flags passed to the zip backend. Must keep `-j`.
#'
#' @return The archive path, invisibly.
#' @noRd
zip_flat <- function(zipfile, files, flags = "-j9X") {
  files <- files[!is.na(files)]
  files <- files[file.exists(files)]
  # A flat archive cannot hold directories; drop them rather than letting the
  # zip backend fail (e.g. a leftover *_files/ folder from saveWidget()).
  files <- files[!dir.exists(files)]

  if (length(files) == 0) {
    stop("Nothing to archive: no existing files were supplied.", call. = FALSE)
  }

  dups <- unique(basename(files)[duplicated(basename(files))])
  if (length(dups) > 0) {
    stop(
      "Cannot flatten archive: duplicated file names (",
      paste(dups, collapse = ", "),
      ").",
      call. = FALSE
    )
  }

  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  zipfile <- normalizePath(zipfile, winslash = "/", mustWork = FALSE)

  status <- utils::zip(zipfile = zipfile, files = files, flags = flags)

  if (!identical(as.integer(status), 0L) || !file.exists(zipfile)) {
    stop(
      "Failed to create the zip archive (zip exit status ", status, "). ",
      "Make sure a `zip` command is available, or set the R_ZIPCMD ",
      "environment variable.",
      call. = FALSE
    )
  }

  invisible(zipfile)
}

#' Per-session directory for generated HTML output
#'
#' Returns a directory that belongs to a single Shiny session, so concurrent
#' users never read or overwrite each other's `synthesis_network.html` or
#' `debate_*.html` files. Outside Shiny (or when `session` is `NULL`) it falls
#' back to a process-unique directory under [tempdir()].
#'
#' @param session A Shiny session object, or `NULL`.
#' @param prefix Subdirectory name under [tempdir()].
#'
#' @return The directory path (created if needed).
#' @noRd
session_html_dir <- function(session = NULL, prefix = "featuremap_html") {
  token <- NULL
  if (!is.null(session) && !is.null(session$token)) {
    token <- as.character(session$token)
  }
  if (is.null(token) || !nzchar(token)) {
    token <- paste0("pid", Sys.getpid())
  }
  token <- gsub("[^A-Za-z0-9._-]+", "_", token)

  dir <- file.path(
    tempdir(),
    prefix,
    paste(format(Sys.time(), "%Y%m%d_%H%M%S"), token, sep = "_")
  )
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(dir)) {
    stop("Could not create the session output directory: ", dir, call. = FALSE)
  }

  dir
}

#' Maximum upload size, in bytes
#'
#' Shiny defaults `shiny.maxRequestSize` to 5 MB, which almost every real
#' metabolomics feature table exceeds -- and a reference database (.rda) is
#' larger still. The limit is a deployment decision, so it is read from the
#' `featuremap.max_upload_mb` option or the `FEATUREMAP_MAX_UPLOAD_MB`
#' environment variable, falling back to `default_mb`.
#'
#' @param default_mb Limit in megabytes when nothing is configured.
#'
#' @return Size in bytes, suitable for `options(shiny.maxRequestSize = )`.
#' @noRd
featuremap_max_upload_bytes <- function(default_mb = 500) {
  configured <- getOption(
    "featuremap.max_upload_mb",
    Sys.getenv("FEATUREMAP_MAX_UPLOAD_MB", unset = "")
  )
  if (is.null(configured) || length(configured) == 0) configured <- ""
  configured <- trimws(as.character(configured[1]))

  mb <- suppressWarnings(as.numeric(configured))
  if (is.na(mb) || mb <= 0) mb <- default_mb

  as.numeric(mb) * 1024^2
}

#' Human-readable form of the current upload limit
#'
#' @return A short string such as `"500 MB"`.
#' @noRd
featuremap_max_upload_label <- function() {
  mb <- featuremap_max_upload_bytes() / 1024^2
  if (mb >= 1024) {
    paste0(format(round(mb / 1024, 1), trim = TRUE), " GB")
  } else {
    paste0(format(round(mb), trim = TRUE), " MB")
  }
}

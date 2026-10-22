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

#' Remove the output directories a session created
#'
#' `session_html_dir()` names every directory `<time>_<session token>`, so
#' the ones a closing session owns can be found again. Called from
#' `session$onSessionEnded()`; without it every visitor's generated HTML
#' stayed on disk until the app restarted.
#'
#' @param session_token The closing session's `session$token`.
#' @param prefix Must match the `prefix` given to `session_html_dir()`.
#' @return Number of directories removed, invisibly.
#' @noRd
featuremap_remove_session_dirs <- function(session_token, prefix = "featuremap_html") {
  token <- gsub("[^A-Za-z0-9._-]+", "_", as.character(session_token %||% ""))
  if (!nzchar(token)) return(invisible(0L))

  root <- file.path(tempdir(), prefix)
  if (!dir.exists(root)) return(invisible(0L))

  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  owned <- dirs[endsWith(basename(dirs), paste0("_", token))]
  unlink(owned, recursive = TRUE, force = TRUE)
  invisible(length(owned))
}

#' Remove session output directories older than a cut-off
#'
#' A safety net for directories whose session never reported its end (for
#' example a dropped connection while the process was busy). Directories are
#' judged by modification time, which changes whenever a run writes into
#' them, so a session still in use is not touched.
#'
#' @param max_age_hours Age limit; defaults to `FEATUREMAP_TEMP_MAX_AGE_HOURS`
#'   or 24.
#' @param prefix Must match the `prefix` given to `session_html_dir()`.
#' @return Number of directories removed, invisibly.
#' @noRd
featuremap_sweep_session_dirs <- function(max_age_hours = NULL, prefix = "featuremap_html") {
  if (is.null(max_age_hours)) {
    max_age_hours <- suppressWarnings(as.numeric(
      Sys.getenv("FEATUREMAP_TEMP_MAX_AGE_HOURS", unset = "24")
    ))
  }
  if (length(max_age_hours) != 1 || is.na(max_age_hours) || max_age_hours <= 0) {
    max_age_hours <- 24
  }

  root <- file.path(tempdir(), prefix)
  if (!dir.exists(root)) return(invisible(0L))

  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0) return(invisible(0L))

  age_hours <- as.numeric(difftime(Sys.time(), file.mtime(dirs), units = "hours"))
  old <- dirs[!is.na(age_hours) & age_hours > max_age_hours]
  unlink(old, recursive = TRUE, force = TRUE)
  invisible(length(old))
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

#' Read a true/false environment variable
#'
#' @param name Environment variable name.
#' @param default Value used when the variable is unset.
#'
#' @return `TRUE` for "true", "1", "yes", "y" or "on" (any case).
#' @noRd
featuremap_env_flag <- function(name, default = "false") {
  value <- tolower(trimws(Sys.getenv(name, unset = default)))
  value %in% c("true", "1", "yes", "y", "on")
}

#' Whether this is a public deployment
#'
#' `FEATUREMAP_PUBLIC_MODE=true` is for a server open to anyone on the
#' internet. It switches on the restrictions such a server needs and that
#' local installs and the lab server do without:
#' - sign-in is required (it implies `FEATUREMAP_AUTH_ENABLED=true`);
#' - LLM and NCBI API keys must be entered by each visitor; keys in the
#'   server's environment are never used, so visitors cannot spend them;
#' - the "Local" LLM provider is hidden and refused, because its base URL
#'   would let visitors make the server send requests to any address,
#'   including machines on the internal network;
#' - each analysis's thread count is capped (see `featuremap_max_threads()`).
#'
#' @return `TRUE` when `FEATUREMAP_PUBLIC_MODE` is set to a true value.
#' @noRd
featuremap_public_mode <- function() {
  featuremap_env_flag("FEATUREMAP_PUBLIC_MODE")
}

#' Whether visitors must sign in
#'
#' @return `TRUE` when `FEATUREMAP_AUTH_ENABLED` is true or the app runs in
#'   public mode.
#' @noRd
featuremap_auth_required <- function() {
  featuremap_public_mode() || featuremap_env_flag("FEATUREMAP_AUTH_ENABLED")
}

#' Upper limit for the per-job "Threads" inputs
#'
#' Each analysis may start its own parallel R workers (the Threads inputs of
#' Steps 2-4), and every one of them loads the package and data, so the
#' memory an analysis needs grows with this number. Set
#' `FEATUREMAP_MAX_THREADS` to change it; the default is 8 in public mode
#' and 16 (the old fixed maximum) otherwise.
#'
#' @return A positive integer.
#' @noRd
featuremap_max_threads <- function() {
  default <- if (featuremap_public_mode()) 8L else 16L
  value <- trimws(Sys.getenv("FEATUREMAP_MAX_THREADS", unset = ""))
  n <- suppressWarnings(as.integer(value))
  if (length(n) != 1 || is.na(n) || n < 1) n <- default
  n
}

#' Clamp a requested thread count to the allowed range
#'
#' The Threads inputs carry a `max`, but a browser can send any number, so
#' the server enforces the limit itself.
#'
#' @param x Requested number of threads.
#' @return An integer between 1 and `featuremap_max_threads()`.
#' @noRd
featuremap_clamp_threads <- function(x) {
  n <- suppressWarnings(as.integer(x))
  if (length(n) != 1 || is.na(n) || n < 1) n <- 1L
  min(n, featuremap_max_threads())
}

#' Fail fast when sign-in is switched on but not set up
#'
#' Sign-in is off by default (local installs and the lab server). A public
#' deployment turns it on with `FEATUREMAP_AUTH_ENABLED=true` or
#' `FEATUREMAP_PUBLIC_MODE=true`. When it is on, the
#' account system needs DBI, RSQLite and sodium (kept in Suggests so local
#' users never have to install libsodium) plus working SMTP settings and a
#' public URL for the links in verification and password-reset emails.
#' Without this check those gaps only surfaced when the first visitor tried
#' to register; now the app refuses to start and says what is missing.
#'
#' `FEATUREMAP_EMAIL_DRY_RUN=true` skips the email checks, for testing the
#' sign-in flow without sending mail.
#'
#' @return `TRUE` invisibly when sign-in is on and ready, `FALSE` when it is
#'   off. Stops with a list of every problem otherwise.
#' @noRd
featuremap_check_auth_setup <- function() {
  if (!featuremap_auth_required()) {
    return(invisible(FALSE))
  }

  problems <- character(0)

  auth_packages <- c("DBI", "RSQLite", "sodium")
  missing_packages <- auth_packages[!vapply(
    auth_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(missing_packages) > 0) {
    problems <- c(problems, paste0(
      "R packages needed for sign-in are not installed: ",
      paste(missing_packages, collapse = ", "),
      ". Install them with install.packages(c(",
      paste0("\"", missing_packages, "\"", collapse = ", "),
      ")). On Linux, sodium also needs the system library libsodium ",
      "(e.g. libsodium-dev)."
    ))
  }

  if (!featuremap_env_flag("FEATUREMAP_EMAIL_DRY_RUN")) {
    password <- Sys.getenv("FEATUREMAP_SMTP_PASSWORD", unset = "")
    if (!nzchar(password)) password <- Sys.getenv("SMTP_PASSWORD", unset = "")
    if (!nzchar(trimws(password))) {
      problems <- c(problems, paste0(
        "FEATUREMAP_SMTP_PASSWORD is not set, so verification and ",
        "password-reset emails cannot be sent. For Gmail use an app password."
      ))
    }

    public_url <- trimws(Sys.getenv("FEATUREMAP_PUBLIC_URL", unset = ""))
    if (!nzchar(public_url)) {
      problems <- c(problems, paste0(
        "FEATUREMAP_PUBLIC_URL is not set, so links in account emails would ",
        "point at the wrong address. Set it to the URL visitors open, ",
        "e.g. https://featuremap.example.org."
      ))
    } else if (!grepl("^https?://", public_url, ignore.case = TRUE)) {
      problems <- c(problems, paste0(
        "FEATUREMAP_PUBLIC_URL must start with http:// or https:// (got \"",
        public_url, "\")."
      ))
    }
  }

  if (length(problems) > 0) {
    stop(
      "Sign-in is switched on (FEATUREMAP_AUTH_ENABLED or FEATUREMAP_PUBLIC_MODE), ",
      "but it is not ready:\n",
      paste0("  - ", problems, collapse = "\n"),
      "\nFix the above, or unset FEATUREMAP_PUBLIC_MODE and set ",
      "FEATUREMAP_AUTH_ENABLED=false to run without sign-in.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

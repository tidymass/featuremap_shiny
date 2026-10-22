#' Background analysis worker
#'
#' The long FeatureMAP steps (feature annotation, PCSF subnetwork extraction,
#' module identification and LLM interpretation) used to run inside the Shiny
#' process itself. Shiny serves every visitor from that one process, so while
#' one person's analysis ran, every other page on the server froze.
#'
#' They now run in a separate, long-lived R process (a `callr::r_session`)
#' shared by all sessions. The Shiny process only handles the interface, so
#' pages stay responsive while an analysis runs. Jobs wait in a first-come,
#' first-served queue kept in the Shiny process, which also lets the
#' interface tell a visitor how many jobs are ahead of theirs.
#'
#' Why callr rather than mirai: metid starts its own parallel R workers with
#' `future::plan(multisession)`. Inside a mirai daemon those workers stalled
#' (0% CPU, no progress); inside a plain callr session they run normally.
#'
#' This is meant for servers shared by several people. A local install keeps
#' the original behaviour (analyses run inside the Shiny process, no extra R
#' process, no extra packages needed).
#'
#' Configuration (environment variables, read when the app starts):
#' - `FEATUREMAP_BG_WORKERS`: number of worker processes, i.e. analyses that
#'   can run at the same time. `0` (the default for local use) runs every job
#'   inside the Shiny process, as before. Public mode defaults to 1; the lab
#'   server sets 1 in deploy/run_app.sh. One worker keeps memory use
#'   predictable (each analysis also starts its own parallel workers, see
#'   `featuremap_max_threads()`).
#'
#' - `FEATUREMAP_MEMORY_LIMIT_GB`: hard cap on the memory of one analysis
#'   (the worker process plus every process it starts, e.g. metid's parallel
#'   workers). Checked every 2 seconds; a job that goes over is stopped, the
#'   visitor is told why, and a fresh worker is started. Unset (the default)
#'   means no cap. Linux only (reads /proc). Each finished job's peak memory
#'   is written to the log, which is how to choose the value.
#'
#' Requires the callr package (Suggests) and shiny >= 1.8.1 (ExtendedTask).
#' If either is missing, the app logs why and falls back to running jobs
#' inside the Shiny process rather than failing to start.
#'
#' @name featuremap_background
#' @keywords internal
#' @noRd
NULL

# Analysis functions a worker is allowed to call. Keeps the job runner from
# becoming a way to call arbitrary functions.
.featuremap_bg_functions <- c(
  "annotate_feature",
  "extract_subnetwork",
  "identify_module",
  "annotate_module"
)

# Process-wide state: the worker processes and the queue. One Shiny process
# serves all sessions, so this is shared by every visitor.
.featuremap_bg <- new.env(parent = emptyenv())
.featuremap_bg$workers <- list()   # each: environment(session, job)
.featuremap_bg$queue <- list()     # jobs waiting for a worker, oldest first
.featuremap_bg$seq <- 0
.featuremap_bg$polling <- FALSE

#' Number of worker processes to start
#'
#' @param default Used when `FEATUREMAP_BG_WORKERS` is unset or invalid:
#'   1 in public mode, otherwise 0 (local installs behave as before).
#' @return A non-negative integer.
#' @noRd
featuremap_bg_pool_size <- function(default = if (featuremap_public_mode()) 1L else 0L) {
  value <- trimws(Sys.getenv("FEATUREMAP_BG_WORKERS", unset = ""))
  n <- suppressWarnings(as.integer(value))
  if (length(n) != 1 || is.na(n) || n < 0) n <- as.integer(default)
  n
}

#' Whether background workers are running
#' @noRd
featuremap_bg_active <- function() {
  length(.featuremap_bg$workers) > 0
}

# Start one worker process. The library paths are passed explicitly: the
# deployment launcher sets .libPaths() in R code, which a child process does
# not inherit.
featuremap_bg_new_session <- function() {
  callr::r_session$new(
    options = callr::r_session_options(
      libpath = .libPaths(),
      env = c(R_FEATUREMAP_BG_WORKER = "true")
    ),
    wait = TRUE
  )
}

#' Start the background worker processes
#'
#' Safe to call more than once: running workers are left as they are.
#'
#' @param n Number of workers; `0` starts nothing (jobs then run inline).
#' @return `TRUE` invisibly when workers are running afterwards.
#' @noRd
featuremap_bg_start <- function(n = featuremap_bg_pool_size()) {
  if (n <= 0) return(invisible(FALSE))
  if (featuremap_bg_active()) return(invisible(TRUE))

  missing <- character(0)
  if (!requireNamespace("callr", quietly = TRUE)) {
    missing <- c(missing, "the callr package is not installed (install.packages(\"callr\"))")
  }
  if (utils::packageVersion("shiny") < "1.8.1") {
    missing <- c(missing, paste0(
      "shiny ", utils::packageVersion("shiny"), " is too old (need >= 1.8.1)"
    ))
  }
  if (length(missing) > 0) {
    message(
      "FeatureMAP: FEATUREMAP_BG_WORKERS=", n, " but background workers are not ",
      "available: ", paste(missing, collapse = "; "),
      ". Running analyses inside the Shiny process instead."
    )
    return(invisible(FALSE))
  }

  .featuremap_bg$workers <- lapply(seq_len(n), function(i) {
    w <- new.env(parent = emptyenv())
    w$session <- featuremap_bg_new_session()
    w$job <- NULL
    w
  })
  invisible(TRUE)
}

#' Stop the background worker processes
#' @noRd
featuremap_bg_stop <- function() {
  for (w in .featuremap_bg$workers) {
    try(w$session$close(grace = 1000), silent = TRUE)
  }
  .featuremap_bg$workers <- list()
  .featuremap_bg$queue <- list()
  invisible(NULL)
}

#' Current load on the background workers
#'
#' @return `NULL` when no workers are running, otherwise a list with
#'   `workers` (number of worker processes), `running` (jobs being evaluated)
#'   and `waiting` (jobs queued for a free worker).
#' @noRd
featuremap_bg_load <- function() {
  if (!featuremap_bg_active()) return(NULL)
  busy <- sum(vapply(.featuremap_bg$workers, function(w) !is.null(w$job), logical(1)))
  list(
    workers = length(.featuremap_bg$workers),
    running = busy,
    waiting = length(.featuremap_bg$queue)
  )
}

#' Location of featuremapshiny for the worker
#'
#' When the app runs from a source checkout via `pkgload::load_all()`, the
#' worker must load the same source directory rather than an installed copy.
#' @noRd
featuremap_bg_package_source <- function() {
  ns <- asNamespace("featuremapshiny")
  dev <- requireNamespace("pkgload", quietly = TRUE) &&
    isTRUE(pkgload::is_dev_package("featuremapshiny"))
  list(path = getNamespaceInfo(ns, "path"), dev = dev)
}

#' Memory cap for one analysis, in bytes
#'
#' @return `NULL` when `FEATUREMAP_MEMORY_LIMIT_GB` is unset or invalid.
#' @noRd
featuremap_bg_memory_limit <- function() {
  gb <- suppressWarnings(as.numeric(Sys.getenv("FEATUREMAP_MEMORY_LIMIT_GB", unset = "")))
  if (length(gb) != 1 || is.na(gb) || gb <= 0) return(NULL)
  gb * 1024^3
}

#' Resident memory of a process and all its descendants, in bytes
#'
#' Reads /proc directly (one pass over all processes), so it is cheap enough
#' to run every couple of seconds. Returns `NA` where /proc is unavailable
#' (macOS, Windows), in which case no cap is enforced.
#'
#' @param pid Root process id.
#' @noRd
featuremap_bg_tree_rss <- function(pid) {
  if (is.null(pid) || is.na(pid) || !dir.exists("/proc")) return(NA_real_)
  pids <- list.files("/proc", pattern = "^[0-9]+$")
  if (length(pids) == 0) return(NA_real_)

  info <- lapply(pids, function(p) {
    line <- tryCatch(readLines(file.path("/proc", p, "stat"), n = 1, warn = FALSE),
                     error = function(e) NULL)
    if (length(line) != 1) return(NULL)
    # The command name is in parentheses and may contain spaces, so split
    # only what follows the last ")": state ppid ... rss is the 22nd field.
    rest <- strsplit(trimws(sub("^.*\\)", "", line)), " +")[[1]]
    if (length(rest) < 22) return(NULL)
    c(pid = as.numeric(p), ppid = as.numeric(rest[2]), rss = as.numeric(rest[22]))
  })
  info <- do.call(rbind, Filter(Negate(is.null), info))
  if (is.null(info)) return(NA_real_)

  tree <- as.numeric(pid)
  repeat {
    children <- info[info[, "ppid"] %in% tree & !(info[, "pid"] %in% tree), "pid"]
    if (length(children) == 0) break
    tree <- c(tree, children)
  }

  page <- .featuremap_bg$page_size
  if (is.null(page)) {
    page <- suppressWarnings(as.numeric(system("getconf PAGESIZE", intern = TRUE)))
    if (length(page) != 1 || is.na(page)) page <- 4096
    .featuremap_bg$page_size <- page
  }
  sum(info[info[, "pid"] %in% tree, "rss"]) * page
}

# Stop a worker process together with everything it started (metid's
# parallel R workers would otherwise be left running), then replace it.
featuremap_bg_replace_worker <- function(w) {
  killed <- tryCatch({ w$session$kill_tree(); TRUE }, error = function(e) FALSE)
  if (!killed) try(w$session$kill(), silent = TRUE)
  w$session <- featuremap_bg_new_session()
  w$checked_at <- NULL
  invisible(NULL)
}

featuremap_bg_gb <- function(bytes) {
  if (is.null(bytes) || is.na(bytes)) "unknown" else sprintf("%.1f GB", bytes / 1024^3)
}

#' Run one analysis job (worker side)
#'
#' Called inside a worker process, or inline when no worker is running.
#' Errors are returned as a `featuremap_bg_error` object rather than thrown,
#' so the interface gets the analysis's own message.
#'
#' @param fun_name One of `.featuremap_bg_functions`.
#' @param args Named list of arguments for that function.
#' @param clean_up Tidy the worker after the job: shut down the parallel R
#'   workers the analysis started (metid leaves `future::plan(multisession)`
#'   in place, and its idle workers would otherwise keep their memory until
#'   the next job) and remove files the job left in `tempdir()`. Only safe in
#'   a worker, which runs one job at a time; never inline, where the Shiny
#'   process's temporary directory is shared with live sessions.
#' @noRd
featuremap_bg_run <- function(fun_name, args, clean_up = FALSE) {
  if (!(fun_name %in% .featuremap_bg_functions)) {
    stop("Unknown background job: ", fun_name, call. = FALSE)
  }

  if (isTRUE(clean_up)) {
    before <- list.files(tempdir(), all.files = TRUE, no.. = TRUE, full.names = TRUE)
    on.exit({
      try(future::plan(future::sequential), silent = TRUE)
      after <- list.files(tempdir(), all.files = TRUE, no.. = TRUE, full.names = TRUE)
      unlink(setdiff(after, before), recursive = TRUE, force = TRUE)
    }, add = TRUE)
  }

  fun <- get(fun_name, envir = asNamespace("featuremapshiny"), mode = "function")
  tryCatch(
    do.call(fun, args),
    error = function(e) {
      structure(
        list(message = conditionMessage(e)),
        class = "featuremap_bg_error"
      )
    }
  )
}

# Function sent to the worker process. callr resets its environment to the
# worker's global environment, so it may only use what it is given and
# `pkg::` calls.
featuremap_bg_worker_call <- function(fun_name, args, path, dev) {
  if (!("featuremapshiny" %in% loadedNamespaces())) {
    if (isTRUE(dev)) {
      pkgload::load_all(path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    } else {
      loadNamespace("featuremapshiny")
    }
  }
  run <- get("featuremap_bg_run", envir = asNamespace("featuremapshiny"))
  run(fun_name, args, clean_up = TRUE)
}

#' Submit an analysis job (Shiny side)
#'
#' @inheritParams featuremap_bg_run
#' @return A job handle: an environment whose `promise` resolves to the
#'   analysis result or a `featuremap_bg_error` (and rejects only if the
#'   worker process itself fails). `started` turns `TRUE` once a worker has
#'   picked the job up. Pass the handle to `featuremap_bg_jobs_ahead()` and
#'   `featuremap_bg_cancel()`.
#' @noRd
featuremap_bg_submit <- function(fun_name, args) {
  if (!(fun_name %in% .featuremap_bg_functions)) {
    stop("Unknown background job: ", fun_name, call. = FALSE)
  }

  job <- new.env(parent = emptyenv())
  .featuremap_bg$seq <- .featuremap_bg$seq + 1
  job$id <- .featuremap_bg$seq
  job$fun_name <- fun_name
  job$args <- args
  job$started <- FALSE
  job$cancelled <- FALSE
  job$promise <- promises::promise(function(resolve, reject) {
    job$resolve <- resolve
    job$reject <- reject
  })

  if (!featuremap_bg_active()) {
    # No workers: run here, blocking this process, as before.
    job$started <- TRUE
    job$resolve(featuremap_bg_run(fun_name, args, clean_up = FALSE))
    return(job)
  }

  .featuremap_bg$queue <- c(.featuremap_bg$queue, list(job))
  featuremap_bg_dispatch()
  featuremap_bg_ensure_polling()
  job
}

# Hand queued jobs to idle workers.
featuremap_bg_dispatch <- function() {
  for (w in .featuremap_bg$workers) {
    if (!is.null(w$job) || length(.featuremap_bg$queue) == 0) next
    job <- .featuremap_bg$queue[[1]]
    .featuremap_bg$queue <- .featuremap_bg$queue[-1]

    if (!isTRUE(w$session$is_alive())) {
      w$session <- featuremap_bg_new_session()
    }

    pkg <- featuremap_bg_package_source()
    sent <- tryCatch({
      w$session$call(
        featuremap_bg_worker_call,
        args = list(fun_name = job$fun_name, args = job$args,
                    path = pkg$path, dev = pkg$dev)
      )
      TRUE
    }, error = function(e) {
      job$reject(simpleError(paste("Could not start the analysis:", conditionMessage(e))))
      FALSE
    })
    job$args <- NULL  # the worker has its own copy now
    if (sent) {
      job$started <- TRUE
      job$peak <- NA_real_
      w$job <- job
      w$checked_at <- NULL
    }
  }
  invisible(NULL)
}

# Collect finished jobs. Runs on the Shiny process's event loop (later) while
# any job is queued or running, so the interface is never blocked waiting.
featuremap_bg_poll <- function() {
  limit <- featuremap_bg_memory_limit()
  for (w in .featuremap_bg$workers) {
    job <- w$job
    if (is.null(job)) next

    # Memory check every 2 seconds: track the peak, and stop the job before
    # it can starve the rest of the server.
    now <- Sys.time()
    if (is.null(w$checked_at) || as.numeric(now - w$checked_at, units = "secs") >= 2) {
      w$checked_at <- now
      rss <- featuremap_bg_tree_rss(w$session$get_pid())
      if (!is.na(rss)) job$peak <- max(job$peak, rss, na.rm = TRUE)
      if (!is.null(limit) && !is.na(rss) && rss > limit) {
        message(
          "FeatureMAP ", job$fun_name, " job stopped: memory ", featuremap_bg_gb(rss),
          " exceeded FEATUREMAP_MEMORY_LIMIT_GB (", featuremap_bg_gb(limit), ")"
        )
        w$job <- NULL
        if (!job$cancelled) {
          job$resolve(structure(
            list(message = paste0(
              "This analysis needed more memory than this server allows (",
              featuremap_bg_gb(limit), "), so it was stopped to keep the server ",
              "running. Try a smaller dataset, or install FeatureMAP on your own ",
              "computer to run it without this limit."
            )),
            class = "featuremap_bg_error"
          ))
        }
        featuremap_bg_replace_worker(w)
        next
      }
    }

    if (w$session$poll_process(0) != "ready") next

    msg <- tryCatch(w$session$read(), error = function(e) {
      list(code = 500, error = e)
    })
    if (is.null(msg)) next

    # Forward the worker's console output, so it still ends up in the app log.
    if (length(msg$stdout) && nzchar(msg$stdout)) cat(msg$stdout)
    if (length(msg$stderr) && nzchar(msg$stderr)) cat(msg$stderr, file = stderr())

    if (identical(as.integer(msg$code), 200L)) {
      w$job <- NULL
      message("FeatureMAP ", job$fun_name, " job finished; peak memory ",
              featuremap_bg_gb(job$peak))
      if (!job$cancelled) {
        if (!is.null(msg$error)) {
          job$resolve(structure(
            list(message = conditionMessage(msg$error)),
            class = "featuremap_bg_error"
          ))
        } else {
          job$resolve(msg$result)
        }
      }
    } else if (as.integer(msg$code) >= 500L) {
      # The worker process died (for example killed for running out of
      # memory). Fail this job and start a fresh worker for the next one.
      w$job <- NULL
      if (!job$cancelled) {
        job$reject(simpleError(paste(
          "The analysis worker stopped unexpectedly",
          "(possibly out of memory). Please try again, or use fewer threads."
        )))
      }
      featuremap_bg_replace_worker(w)
    }
    # Other codes (e.g. 301, progress messages) need no action.
  }
  featuremap_bg_dispatch()
}

featuremap_bg_busy <- function() {
  length(.featuremap_bg$queue) > 0 ||
    any(vapply(.featuremap_bg$workers, function(w) !is.null(w$job), logical(1)))
}

featuremap_bg_ensure_polling <- function() {
  if (isTRUE(.featuremap_bg$polling)) return(invisible(NULL))
  .featuremap_bg$polling <- TRUE
  tick <- function() {
    try(featuremap_bg_poll(), silent = FALSE)
    if (featuremap_bg_busy()) {
      later::later(tick, 0.5)
    } else {
      .featuremap_bg$polling <- FALSE
    }
  }
  later::later(tick, 0.5)
  invisible(NULL)
}

#' Number of unfinished jobs ahead of this one
#'
#' Counts jobs being evaluated plus jobs queued before this one. With one
#' worker, `0` means this job is the one running.
#'
#' @param job From `featuremap_bg_submit()`.
#' @noRd
featuremap_bg_jobs_ahead <- function(job) {
  if (is.null(job) || isTRUE(job$started)) return(0L)
  pos <- which(vapply(.featuremap_bg$queue, function(j) identical(j, job), logical(1)))
  if (length(pos) == 0) return(0L)
  running <- sum(vapply(.featuremap_bg$workers, function(w) !is.null(w$job), logical(1)))
  as.integer(running + pos - 1L)
}

#' Cancel a submitted job
#'
#' Used when a session closes with its analysis still queued or running, so
#' an abandoned job does not keep the worker busy.
#'
#' @param job From `featuremap_bg_submit()`.
#' @noRd
featuremap_bg_cancel <- function(job) {
  if (is.null(job) || !is.environment(job)) return(invisible(NULL))
  job$cancelled <- TRUE

  # Still queued: just drop it.
  .featuremap_bg$queue <- Filter(function(j) !identical(j, job), .featuremap_bg$queue)

  # Running: stop the worker's current evaluation. Interrupting can leave
  # the analysis's own parallel workers behind, so replace the process.
  for (w in .featuremap_bg$workers) {
    if (identical(w$job, job)) {
      w$job <- NULL
      featuremap_bg_replace_worker(w)
    }
  }
  featuremap_bg_dispatch()
  invisible(NULL)
}

#' Message for a failed job
#'
#' @param x A `featuremap_bg_error` or a condition.
#' @return A single string for the interface.
#' @noRd
featuremap_bg_error_message <- function(x) {
  if (inherits(x, "featuremap_bg_error")) return(x$message)
  msg <- if (inherits(x, "condition")) conditionMessage(x) else paste(as.character(x), collapse = " ")
  msg <- trimws(sub("^Error( in .*?)? ?: *", "", msg, perl = TRUE))
  if (!nzchar(msg)) msg <- "The background job failed without a message."
  msg
}

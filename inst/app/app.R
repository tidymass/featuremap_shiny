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

# Shiny's 5 MB default rejects almost every real feature table, and reference
# databases are larger still. Override with the featuremap.max_upload_mb
# option or the FEATUREMAP_MAX_UPLOAD_MB environment variable.
options(shiny.maxRequestSize = featuremap_max_upload_bytes())

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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

featuremap_auth_enabled <- function() {
  value <- tolower(trimws(Sys.getenv("FEATUREMAP_AUTH_ENABLED", unset = "false")))
  value %in% c("true", "1", "yes", "y", "on")
}

featuremap_truthy <- function(value, default = "false") {
  value <- tolower(trimws(Sys.getenv(value, unset = default)))
  value %in% c("true", "1", "yes", "y", "on")
}

featuremap_auth_now <- function() {
  as.numeric(Sys.time())
}

featuremap_auth_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
}

featuremap_auth_generate_token <- function() {
  token <- openssl::base64_encode(openssl::rand_bytes(32), linebreaks = FALSE)
  token <- chartr("+/", "-_", token)
  gsub("=+$", "", token)
}

featuremap_auth_token_hash <- function(token) {
  digest::digest(as.character(token %||% ""), algo = "sha256", serialize = FALSE)
}

featuremap_auth_email_config <- function() {
  username <- Sys.getenv("FEATUREMAP_SMTP_USERNAME", unset = "")
  if (!nzchar(username)) username <- Sys.getenv("SMTP_USERNAME", unset = "featuremapofficial@gmail.com")
  password <- Sys.getenv("FEATUREMAP_SMTP_PASSWORD", unset = "")
  if (!nzchar(password)) password <- Sys.getenv("SMTP_PASSWORD", unset = "")
  if (grepl("gmail", Sys.getenv("FEATUREMAP_SMTP_HOST", unset = Sys.getenv("SMTP_HOST", unset = "smtp.gmail.com")), ignore.case = TRUE)) {
    password <- gsub("\\s+", "", password)
  }
  from <- Sys.getenv("FEATUREMAP_SMTP_FROM", unset = "")
  if (!nzchar(from)) from <- username

  list(
    host = Sys.getenv("FEATUREMAP_SMTP_HOST", unset = Sys.getenv("SMTP_HOST", unset = "smtp.gmail.com")),
    port = Sys.getenv("FEATUREMAP_SMTP_PORT", unset = Sys.getenv("SMTP_PORT", unset = "587")),
    username = username,
    password = password,
    from = from,
    dry_run = featuremap_truthy("FEATUREMAP_EMAIL_DRY_RUN")
  )
}

featuremap_auth_header_value <- function(value) {
  gsub("[\r\n]+", " ", as.character(value %||% ""))
}

featuremap_auth_send_mail <- function(to, subject, body) {
  cfg <- featuremap_auth_email_config()
  to <- featuremap_auth_normalize_email(to)
  if (!featuremap_auth_validate_email(to)) {
    stop("Email address is invalid.", call. = FALSE)
  }
  smtp_url <- sprintf("smtp://%s:%s", cfg$host, cfg$port)
  if (identical(cfg$port, "465")) smtp_url <- sprintf("smtps://%s:%s", cfg$host, cfg$port)
  message <- paste0(
    "From: FeatureMAP <", featuremap_auth_header_value(cfg$from), ">\r\n",
    "To: ", featuremap_auth_header_value(to), "\r\n",
    "Subject: ", featuremap_auth_header_value(subject), "\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: text/plain; charset=\"UTF-8\"\r\n",
    "\r\n",
    body
  )

  if (isTRUE(cfg$dry_run)) {
    return(invisible(list(sent = FALSE, dry_run = TRUE, message = message)))
  }

  if (!nzchar(cfg$username) || !nzchar(cfg$password)) {
    stop("SMTP is not configured. Set FEATUREMAP_SMTP_PASSWORD before sending account emails.", call. = FALSE)
  }

  result <- tryCatch(
    curl::send_mail(
      mail_from = cfg$from,
      mail_rcpt = to,
      message = message,
      smtp_server = smtp_url,
      use_ssl = "force",
      username = cfg$username,
      password = cfg$password,
      verbose = FALSE
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    message <- conditionMessage(result)
    if (grepl("login denied|authentication|invalid username|invalid password", message, ignore.case = TRUE)) {
      stop(
        "Gmail SMTP login failed. Check FEATUREMAP_SMTP_USERNAME and FEATUREMAP_SMTP_PASSWORD; use the Gmail app password, not the normal Gmail password.",
        call. = FALSE
      )
    }
    stop(message, call. = FALSE)
  }
  invisible(list(sent = TRUE, dry_run = FALSE))
}

featuremap_auth_send_verification_email <- function(to, token, base_url) {
  link <- paste0(sub("/+$", "", base_url), "/?verify_token=", utils::URLencode(token, reserved = TRUE))
  body <- paste0(
    "Welcome to FeatureMAP!\r\n\r\n",
    "Click the link below to verify your email address:\r\n",
    link, "\r\n\r\n",
    "This link expires in 24 hours. If you didn't sign up, you can ignore this email.\r\n"
  )
  featuremap_auth_send_mail(to, "Verify your FeatureMAP account", body)
}

featuremap_auth_send_password_reset_email <- function(to, token, base_url) {
  link <- paste0(sub("/+$", "", base_url), "/?reset_token=", utils::URLencode(token, reserved = TRUE))
  body <- paste0(
    "We received a request to reset your FeatureMAP password.\r\n\r\n",
    "Click the link below to choose a new password:\r\n",
    link, "\r\n\r\n",
    "This link expires in 1 hour. If you didn't request this, you can ignore this email.\r\n"
  )
  featuremap_auth_send_mail(to, "Reset your FeatureMAP password", body)
}

featuremap_auth_db_path <- function() {
  env_path <- Sys.getenv("FEATUREMAP_AUTH_DB", unset = "")
  if (nzchar(trimws(env_path))) {
    return(path.expand(env_path))
  }

  # Never create account data inside the installed package or application
  # directory. Those locations may be read-only, replaced during deployment,
  # or accidentally included when the source directory is copied elsewhere.
  file.path(
    tools::R_user_dir("featuremapshiny", which = "data"),
    "featuremap_auth.sqlite"
  )
}

featuremap_auth_connect <- function() {
  db_path <- featuremap_auth_db_path()
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  featuremap_auth_init(con)
  con
}

featuremap_auth_init <- function(con) {
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS users (
       id TEXT PRIMARY KEY,
       email TEXT UNIQUE NOT NULL,
       display_name TEXT NOT NULL,
       institution TEXT NOT NULL DEFAULT '',
       country_region TEXT NOT NULL DEFAULT '',
       password_hash TEXT NOT NULL,
       role TEXT NOT NULL DEFAULT 'user',
       email_verified INTEGER NOT NULL DEFAULT 1,
       created_at TEXT NOT NULL,
       updated_at TEXT NOT NULL,
       last_login_at TEXT
     )"
  )
  featuremap_auth_add_column_if_missing(con, "users", "email_verified", "INTEGER NOT NULL DEFAULT 1")
  featuremap_auth_add_column_if_missing(con, "users", "updated_at", "TEXT")
  featuremap_auth_add_column_if_missing(con, "users", "last_login_at", "TEXT")
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS email_verifications (
       token_hash TEXT PRIMARY KEY,
       user_id TEXT NOT NULL,
       created_at REAL NOT NULL,
       expires_at REAL NOT NULL,
       used_at REAL
     )"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS password_resets (
       token_hash TEXT PRIMARY KEY,
       user_id TEXT NOT NULL,
       created_at REAL NOT NULL,
       expires_at REAL NOT NULL,
       used_at REAL
     )"
  )
  invisible(TRUE)
}

featuremap_auth_add_column_if_missing <- function(con, table, column, definition) {
  info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))
  if (column %in% info$name) return(invisible(FALSE))
  DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, column, definition))
  invisible(TRUE)
}

featuremap_auth_random_id <- function(prefix = "usr") {
  chars <- c(0:9, letters[1:6])
  paste0(prefix, "_", paste(sample(chars, 32, replace = TRUE), collapse = ""))
}

featuremap_auth_normalize_email <- function(email) {
  tolower(trimws(as.character(email %||% "")))
}

featuremap_auth_validate_email <- function(email) {
  grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email, perl = TRUE)
}

featuremap_auth_password_message <- function() {
  "Password must be at least 8 characters and include at least 3 of: uppercase letters, lowercase letters, numbers, and symbols."
}

featuremap_auth_password_score <- function(password) {
  password <- as.character(password %||% "")
  checks <- c(
    grepl("[A-Z]", password, perl = TRUE),
    grepl("[a-z]", password, perl = TRUE),
    grepl("[0-9]", password, perl = TRUE),
    grepl("[^A-Za-z0-9]", password, perl = TRUE)
  )
  sum(checks)
}

featuremap_auth_validate_password <- function(password) {
  is.character(password) &&
    length(password) == 1 &&
    nchar(password) >= 8 &&
    featuremap_auth_password_score(password) >= 3
}

featuremap_auth_token_cooldown_remaining <- function(con, table, user_id, cooldown_seconds = 60) {
  if (is.null(cooldown_seconds) || cooldown_seconds <= 0) return(0L)
  row <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT MAX(created_at) AS created_at
         FROM %s
        WHERE user_id = ?
          AND used_at IS NULL",
      table
    ),
    params = list(user_id)
  )
  if (nrow(row) < 1 || is.na(row$created_at[[1]])) return(0L)
  last_created <- suppressWarnings(as.numeric(row$created_at[[1]]))
  if (is.na(last_created)) return(0L)
  max(0L, ceiling(cooldown_seconds - (featuremap_auth_now() - last_created)))
}

featuremap_auth_user_from_row <- function(row) {
  if (is.null(row) || nrow(row) < 1) return(NULL)
  row <- row[1, , drop = FALSE]
  list(
    id = row$id[[1]],
    email = row$email[[1]],
    display_name = row$display_name[[1]],
    institution = row$institution[[1]],
    country_region = row$country_region[[1]],
    role = row$role[[1]],
    email_verified = isTRUE(as.integer(row$email_verified[[1]]) == 1L)
  )
}

featuremap_auth_find_user <- function(con, email) {
  DBI::dbGetQuery(
    con,
    "SELECT id, email, display_name, institution, country_region, password_hash, role, email_verified
       FROM users
      WHERE email = ?
      LIMIT 1",
    params = list(email)
  )
}

featuremap_auth_find_user_by_id <- function(con, user_id) {
  DBI::dbGetQuery(
    con,
    "SELECT id, email, display_name, institution, country_region, password_hash, role, email_verified
       FROM users
      WHERE id = ?
      LIMIT 1",
    params = list(user_id)
  )
}

featuremap_auth_create_token <- function(con, table, user_id, ttl_seconds) {
  now <- featuremap_auth_now()
  token <- featuremap_auth_generate_token()
  DBI::dbExecute(
    con,
    sprintf("DELETE FROM %s WHERE user_id = ? OR expires_at < ? OR used_at IS NOT NULL", table),
    params = list(user_id, now)
  )
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO %s (token_hash, user_id, created_at, expires_at, used_at)
       VALUES (?, ?, ?, ?, NULL)",
      table
    ),
    params = list(featuremap_auth_token_hash(token), user_id, now, now + ttl_seconds)
  )
  token
}

featuremap_auth_lookup_token <- function(con, table, token) {
  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT token_hash, user_id, created_at, expires_at, used_at
         FROM %s
        WHERE token_hash = ?
          AND used_at IS NULL
          AND expires_at >= ?
        LIMIT 1",
      table
    ),
    params = list(featuremap_auth_token_hash(token), featuremap_auth_now())
  )
}

featuremap_auth_signup <- function(display_name, email, password,
                                   institution = "", country_region = "") {
  display_name <- trimws(as.character(display_name %||% ""))
  email <- featuremap_auth_normalize_email(email)
  password <- as.character(password %||% "")
  institution <- trimws(as.character(institution %||% ""))
  country_region <- trimws(as.character(country_region %||% ""))

  if (!nzchar(display_name)) {
    stop("Please enter your name.", call. = FALSE)
  }
  if (!featuremap_auth_validate_email(email)) {
    stop("Please enter a valid email address.", call. = FALSE)
  }
  if (!nzchar(institution)) {
    stop("Please enter your institution.", call. = FALSE)
  }
  if (!nzchar(country_region)) {
    stop("Please enter your country/region.", call. = FALSE)
  }
  if (!featuremap_auth_validate_password(password)) {
    stop(featuremap_auth_password_message(), call. = FALSE)
  }

  con <- featuremap_auth_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  existing <- featuremap_auth_find_user(con, email)
  if (nrow(existing) > 0) {
    stop("This email is already registered. Please sign in instead.", call. = FALSE)
  }

  now <- featuremap_auth_timestamp()
  user_id <- featuremap_auth_random_id()
  DBI::dbExecute(
    con,
    "INSERT INTO users
       (id, email, display_name, institution, country_region, password_hash, role, email_verified, created_at, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, 'user', 0, ?, ?)",
    params = list(
      user_id,
      email,
      display_name,
      institution,
      country_region,
      sodium::password_store(password),
      now,
      now
    )
  )

  row <- featuremap_auth_find_user(con, email)
  user <- featuremap_auth_user_from_row(row)
  user$verification_token <- featuremap_auth_create_token(con, "email_verifications", user_id, 24 * 60 * 60)
  user
}

featuremap_auth_login <- function(email, password) {
  email <- featuremap_auth_normalize_email(email)
  password <- as.character(password %||% "")

  if (!featuremap_auth_validate_email(email) || !nzchar(password)) {
    stop("Please enter your email and password.", call. = FALSE)
  }

  con <- featuremap_auth_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- featuremap_auth_find_user(con, email)
  if (nrow(row) < 1 || !sodium::password_verify(row$password_hash[[1]], password)) {
    stop("Email or password is incorrect.", call. = FALSE)
  }
  if (!isTRUE(as.integer(row$email_verified[[1]]) == 1L)) {
    stop("Please verify your email before signing in.", call. = FALSE)
  }

  now <- featuremap_auth_timestamp()
  DBI::dbExecute(
    con,
    "UPDATE users SET last_login_at = ?, updated_at = ? WHERE email = ?",
    params = list(now, now, email)
  )

  featuremap_auth_user_from_row(row)
}

featuremap_auth_verify_email <- function(token) {
  token <- trimws(as.character(token %||% ""))
  if (!nzchar(token)) stop("Verification link is invalid or expired.", call. = FALSE)

  con <- featuremap_auth_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- featuremap_auth_lookup_token(con, "email_verifications", token)
  if (nrow(row) < 1) {
    stop("Verification link is invalid or expired.", call. = FALSE)
  }

  user_id <- row$user_id[[1]]
  DBI::dbWithTransaction(con, {
    now <- featuremap_auth_now()
    DBI::dbExecute(
      con,
      "UPDATE users SET email_verified = 1, updated_at = ? WHERE id = ?",
      params = list(featuremap_auth_timestamp(), user_id)
    )
    DBI::dbExecute(
      con,
      "UPDATE email_verifications SET used_at = ? WHERE token_hash = ?",
      params = list(now, row$token_hash[[1]])
    )
    DBI::dbExecute(
      con,
      "DELETE FROM email_verifications WHERE user_id = ? AND token_hash <> ?",
      params = list(user_id, row$token_hash[[1]])
    )
  })

  featuremap_auth_user_from_row(featuremap_auth_find_user_by_id(con, user_id))
}

featuremap_auth_resend_verification <- function(email) {
  email <- featuremap_auth_normalize_email(email)
  if (!featuremap_auth_validate_email(email)) return(NULL)

  con <- featuremap_auth_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- featuremap_auth_find_user(con, email)
  if (nrow(row) < 1 || isTRUE(as.integer(row$email_verified[[1]]) == 1L)) {
    return(NULL)
  }

  user <- featuremap_auth_user_from_row(row)
  remaining <- featuremap_auth_token_cooldown_remaining(con, "email_verifications", user$id, 60)
  if (remaining > 0) {
    stop(sprintf("Please wait %s seconds before requesting another verification email.", remaining), call. = FALSE)
  }
  user$verification_token <- featuremap_auth_create_token(con, "email_verifications", user$id, 24 * 60 * 60)
  user
}

featuremap_auth_request_password_reset <- function(email) {
  email <- featuremap_auth_normalize_email(email)
  if (!featuremap_auth_validate_email(email)) return(NULL)

  con <- featuremap_auth_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- featuremap_auth_find_user(con, email)
  if (nrow(row) < 1) return(NULL)

  user <- featuremap_auth_user_from_row(row)
  remaining <- featuremap_auth_token_cooldown_remaining(con, "password_resets", user$id, 60)
  if (remaining > 0) {
    stop(sprintf("Please wait %s seconds before requesting another password reset email.", remaining), call. = FALSE)
  }
  user$reset_token <- featuremap_auth_create_token(con, "password_resets", user$id, 60 * 60)
  user
}

featuremap_auth_reset_password <- function(token, password) {
  token <- trimws(as.character(token %||% ""))
  password <- as.character(password %||% "")
  if (!nzchar(token)) stop("Reset link is invalid or expired.", call. = FALSE)
  if (!featuremap_auth_validate_password(password)) {
    stop(featuremap_auth_password_message(), call. = FALSE)
  }

  con <- featuremap_auth_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- featuremap_auth_lookup_token(con, "password_resets", token)
  if (nrow(row) < 1) {
    stop("Reset link is invalid or expired.", call. = FALSE)
  }

  user_id <- row$user_id[[1]]
  DBI::dbWithTransaction(con, {
    now <- featuremap_auth_now()
    DBI::dbExecute(
      con,
      "UPDATE users SET password_hash = ?, updated_at = ? WHERE id = ?",
      params = list(sodium::password_store(password), featuremap_auth_timestamp(), user_id)
    )
    DBI::dbExecute(
      con,
      "UPDATE password_resets SET used_at = ? WHERE token_hash = ?",
      params = list(now, row$token_hash[[1]])
    )
    DBI::dbExecute(
      con,
      "DELETE FROM password_resets WHERE user_id = ? AND token_hash <> ?",
      params = list(user_id, row$token_hash[[1]])
    )
  })

  featuremap_auth_user_from_row(featuremap_auth_find_user_by_id(con, user_id))
}

auth_required_label <- function(label, required = FALSE) {
  label_node <- if (inherits(label, "shiny.tag") || inherits(label, "shiny.tag.list")) label else tags$span(label)
  if (!isTRUE(required)) return(label_node)
  tags$span(class = "auth-required-label", label_node, tags$span(class = "auth-required-mark", "*"))
}

auth_text_field <- function(input_id, label, icon_name, type = "text", autocomplete = NULL,
                            required = FALSE, label_required = required) {
  label_node <- if (inherits(label, "shiny.tag") || inherits(label, "shiny.tag.list")) label else tags$span(label)
  tags$label(
    class = "auth-field",
    auth_required_label(label_node, label_required),
    tags$div(
      class = "auth-input-wrap",
      icon(icon_name),
      tags$input(
        id = input_id,
        type = type,
        class = "form-control",
        autocomplete = autocomplete %||% NULL,
        required = if (isTRUE(required)) "required" else NULL,
        `aria-required` = if (isTRUE(required)) "true" else NULL
      )
    )
  )
}

auth_password_field <- function(input_id, label, autocomplete = NULL, required = FALSE,
                                label_required = required, hint = NULL) {
  label_node <- if (inherits(label, "shiny.tag") || inherits(label, "shiny.tag.list")) label else tags$span(label)
  tags$label(
    class = "auth-field",
    auth_required_label(label_node, label_required),
    tags$div(
      class = "auth-input-wrap",
      icon("lock"),
      tags$input(
        id = input_id,
        type = "password",
        class = "form-control has-trailing-btn",
        autocomplete = autocomplete %||% NULL,
        required = if (isTRUE(required)) "required" else NULL,
        `aria-required` = if (isTRUE(required)) "true" else NULL
      ),
      tags$button(
        type = "button",
        class = "auth-password-toggle",
        `data-target` = input_id,
        `aria-label` = "Show or hide password",
        onclick = "(function(btn) {
          var input = document.getElementById(btn.getAttribute('data-target'));
          if (!input) return;
          input.type = input.type === 'password' ? 'text' : 'password';
        })(this);",
        icon("eye")
      )
    ),
    if (!is.null(hint) && nzchar(hint)) tags$span(class = "auth-field-hint", hint)
  )
}

auth_button_press_js <- "this.classList.add('is-auth-pressing'); setTimeout(function(btn) { btn.classList.remove('is-auth-pressing'); }, 260, this);"


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
    tags$script(HTML("
      window.FeatureMAPAuthCooldown = function(id, label, seconds) {
        var button = document.getElementById(id);
        if (!button) return;
        if (button.__featuremapCooldownTimer) {
          clearInterval(button.__featuremapCooldownTimer);
        }
        var remaining = Math.max(0, parseInt(seconds, 10) || 0);
        var setState = function() {
          if (remaining > 0) {
            button.disabled = true;
            button.classList.add('disabled');
            button.textContent = label + ' (' + remaining + 's)';
          } else {
            button.disabled = false;
            button.classList.remove('disabled');
            button.textContent = label;
            clearInterval(button.__featuremapCooldownTimer);
            button.__featuremapCooldownTimer = null;
          }
        };
        setState();
        button.__featuremapCooldownTimer = setInterval(function() {
          remaining -= 1;
          setState();
        }, 1000);
      };
    ")),
    
    tags$style(HTML("
    
    #shiny-notification-panel {
        position: fixed !important;
        top: auto !important;
        left: auto !important;
        right: 24px !important;
        bottom: 24px !important;
        transform: none !important;
        width: auto !important;
        min-width: 280px !important;
        max-width: min(680px, calc(100vw - 48px)) !important;
        z-index: 3000 !important;
      }

    #shiny-notification-panel .shiny-notification {
        display: inline-block !important;
        width: auto !important;
        min-width: 280px !important;
        max-width: min(680px, calc(100vw - 48px)) !important;
        margin: 0 0 12px 0 !important;
        padding: 14px 18px !important;
        border-radius: 12px;
        border: 1px solid rgba(190, 207, 224, 0.92);
        box-shadow: 0 22px 54px rgba(31, 54, 82, 0.26);
        white-space: normal !important;
        overflow-wrap: break-word;
      }

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
      
      /* The running / success / error faces all live in the card from the
         start; only the class decides which one is visible. */
      .fm-process-modal .fm-modal-icon.running,
      .fm-process-modal .fm-modal-icon.error,
      .fm-process-modal .wb-check,
      .fm-process-modal .wb-done-rows,
      .fm-process-modal .fm-modal-actions { display: none !important; }
      .fm-process-modal.is-running > .fm-modal-icon.running { display: flex !important; }
      .fm-process-modal.is-error   > .fm-modal-icon.error   { display: flex !important; }
      .fm-process-modal.is-error   > .fm-modal-actions      { display: flex !important; }
      .fm-process-modal.is-success > .wb-check              { display: block !important; }
      /* keep the icon block the same height in both states so the card
         does not jump when it switches from running to success */
      .fm-process-modal .wb-check { margin: 6px auto 24px auto; }
      .fm-process-modal.is-success > .fm-modal-actions      { display: flex !important; }
      .fm-process-modal.is-success > .wb-done-rows:not(:empty) { display: flex !important; }
      .fm-process-modal.is-running > .wb-check,
      .fm-process-modal.is-running > .wb-done-rows,
      .fm-process-modal.is-running > .fm-modal-actions,
      .fm-process-modal.is-success > .fm-modal-icon.running,
      .fm-process-modal.is-success > .fm-modal-icon.error {
        display: none !important;
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
        position: fixed;
        top: 0;
        left: 0;
        z-index: 2200;
        width: 320px;
        max-width: calc(100vw - 32px);
        padding: 10px 12px;
        border-radius: 10px;
        border: 1px solid #d7e1ec;
        background: #ffffff;
        box-shadow: 0 14px 30px rgba(28, 49, 74, 0.16);
        color: #415366;
        font-size: 12px;
        line-height: 1.5;
        white-space: normal;
        /* the icon can sit inside an uppercase/tracked heading, so the
           popover has to reset the inherited type styling */
        text-transform: none;
        letter-spacing: normal;
        font-weight: 400;
        text-align: left;
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
        



        #network_page #network_result_card {
          display: flex;
          flex-direction: column;
        }







        #network_page #pcsf_network_container {
          display: flex;
          flex: 1;
          min-height: 0;
        }

        #network_page .pcsf-network-shell {
          display: flex;
          flex-direction: column;
          flex: 1 1 auto;
          gap: 12px;
          min-height: 0;
          padding: 16px 26px 20px 26px;
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
          min-height: 460px;
          width: 100%;
          border: 1px solid #e6edf4;
          border-radius: 10px;
          overflow: hidden;
        }

        #network_page .pcsf-network-stage > .empty-panel-fill,
        #network_page .pcsf-network-stage > .visNetwork,
        #network_page .pcsf-network-stage > .html-widget,
        #network_page .pcsf-network-stage > .shiny-vis-network {
          flex: 1;
          min-height: 100%;
          height: 100%;
          width: 100%;
        }

        /* Reproducibility filters, floating over the top-left of the canvas */
        #network_page .pcsf-network-stage {
          position: relative;
        }

        #network_page .pcsf-filter-panel {
          position: absolute;
          top: 12px;
          left: 12px;
          z-index: 10;
          width: 186px;
          padding: 12px 14px 13px 14px;
          background: rgba(255, 255, 255, 0.94);
          -webkit-backdrop-filter: blur(6px);
          backdrop-filter: blur(6px);
          border: 1px solid #e3eaf2;
          border-radius: 10px;
          box-shadow: 0 6px 18px rgba(28, 52, 80, 0.10);
        }

        #network_page .pcsf-filter-head {
          display: flex;
          align-items: center;
          gap: 7px;
          margin-bottom: 11px;
          font-size: 10.5px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #6d8299;
        }

        #network_page .pcsf-filter-head i {
          font-size: 10px;
          color: #9fb2c6;
        }

        #network_page .pcsf-filter-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px;
        }

        #network_page .pcsf-filter-row + .pcsf-filter-row {
          margin-top: 8px;
        }

        #network_page .pcsf-filter-label {
          font-size: 12px;
          font-weight: 600;
          color: #5c7186;
          white-space: nowrap;
        }

        #network_page .pcsf-filter-row .form-group {
          margin: 0;
          flex: 0 0 74px;
          width: 74px;
        }

        #network_page .pcsf-filter-row .form-control {
          height: 30px;
          padding: 3px 8px;
          font-size: 12px;
          font-weight: 600;
          text-align: center;
          color: #26415e;
          border: 1px solid #d7e0ea;
          border-radius: 6px;
          font-variant-numeric: tabular-nums;
        }

        #network_page .pcsf-filter-count {
          margin-top: 11px;
          padding-top: 10px;
          border-top: 1px solid #eef2f7;
          font-size: 11.5px;
          line-height: 1.6;
          color: #8296ab;
          font-variant-numeric: tabular-nums;
        }

        #network_page .pcsf-filter-count b {
          color: #26415e;
          font-weight: 700;
        }

        #network_page .pcsf-filter-note {
          margin-top: 3px;
          font-size: 11px;
          color: #b0bccb;
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
          align-items: center;
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

        /* A long compound name used to wrap onto three lines and push the
           whole topbar (and the canvas) down. Keep the box one line high and
           clip what does not fit - the full name is in the tooltip. */
        #network_page .pcsf-selector-field .selectize-input {
          display: flex;
          align-items: center;
          flex-wrap: nowrap;
          min-height: 38px;
          height: 38px;
          padding-right: 26px;
          overflow: hidden;
        }

        #network_page .pcsf-selector-field .selectize-input > .item,
        #network_page .pcsf-selector-field .selectize-input > input {
          display: block;
          min-width: 0;
          max-width: 100%;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }

        /* the dropdown list itself can stay one line per option */
        #network_page .pcsf-selector-field .selectize-dropdown .option {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }

        #network_page .pcsf-selector-input .inline-info-wrap {
          align-self: center;
        }

        #network_page .pcsf-selector-input .inline-info-wrap {
          margin-left: 0;
          margin-top: 0;
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












        /* =================================================================
           Analysis workbench layout (Step pages)
           Full-bleed, edge-to-edge: header rail + sidebar + work area.
           No floating cards, no outer gutters.
           ================================================================= */

        .workbench {
          display: flex;
          flex-direction: column;
          height: calc(100vh - var(--fm-nav-h, 70px));
          min-height: 520px;
          background: #ffffff;
        }

        /* ---- Header rail ---- */
        .wb-header {
          flex: 0 0 auto;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 32px;
          padding: 13px 28px;
          background: #ffffff;
          border-bottom: 1px solid #e3eaf2;
        }

        .wb-header-left {
          display: flex;
          align-items: center;
          gap: 14px;
          min-width: 0;
        }

        .wb-eyebrow {
          display: inline-flex;
          align-items: center;
          gap: 7px;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.09em;
          text-transform: uppercase;
          color: #8296ab;
          white-space: nowrap;
          flex: 0 0 auto;
        }

        .wb-eyebrow i {
          font-size: 11px;
          color: #9fb2c6;
        }

        .wb-header-divider {
          width: 1px;
          height: 20px;
          background: #e0e7ef;
          flex: 0 0 auto;
        }

        /* Frosted-glass capsule: a translucent tinted pane, a light top
           edge and a soft bloom underneath, sitting on a blurred backdrop. */
        .wb-title {
          position: relative;
          margin: 0;
          padding: 7px 20px;
          border-radius: 999px;
          font-size: 17px;
          font-weight: 800;
          letter-spacing: 0.005em;
          color: #1c4370;
          white-space: nowrap;
          background:
            linear-gradient(140deg, rgba(214, 232, 247, 0.46) 0%,
                                     rgba(194, 219, 240, 0.40) 45%,
                                     rgba(186, 224, 229, 0.44) 100%);
          border: 1px solid rgba(255, 255, 255, 0.55);
          -webkit-backdrop-filter: blur(12px) saturate(165%);
          backdrop-filter: blur(12px) saturate(165%);
          box-shadow:
            0 1px 0 rgba(255, 255, 255, 0.75) inset,
            0 -1px 0 rgba(23, 55, 90, 0.10) inset,
            0 0 0 1px rgba(35, 78, 124, 0.12),
            0 5px 15px rgba(28, 62, 100, 0.15);
          text-shadow: 0 1px 0 rgba(255, 255, 255, 0.35);
        }

        /* the glossy highlight across the upper half */
        .wb-title::before {
          content: \"\";
          position: absolute;
          left: 6px;
          right: 6px;
          top: 1px;
          height: 45%;
          border-radius: 999px 999px 40% 40% / 999px 999px 100% 100%;
          background: linear-gradient(180deg, rgba(255, 255, 255, 0.66), rgba(255, 255, 255, 0));
          pointer-events: none;
        }

        /* a soft tinted bloom so the glass reads as lit from behind */
        .wb-header-left {
          position: relative;
        }

        .wb-header-left::after {
          content: \"\";
          position: absolute;
          left: 45%;
          top: 50%;
          width: 220px;
          height: 34px;
          transform: translateY(-50%);
          border-radius: 999px;
          background: radial-gradient(closest-side, rgba(79, 164, 166, 0.34), transparent 70%);
          filter: blur(12px);
          pointer-events: none;
          z-index: -1;
        }

        /* ---- Step rail ---- */
        .wb-stepper {
          display: flex;
          align-items: center;
          gap: 0;
          margin: 0;
          padding: 0;
          list-style: none;
          flex: 0 0 auto;
        }

        .wb-step {
          display: flex;
          align-items: center;
          gap: 8px;
        }

        .wb-step-dot {
          width: 23px;
          height: 23px;
          border-radius: 50%;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          font-size: 11.5px;
          font-weight: 700;
          background: #eef2f7;
          color: #9aabbd;
          border: 1px solid #e3e9f0;
          flex: 0 0 auto;
        }

        .wb-step-name {
          font-size: 11.5px;
          font-weight: 600;
          letter-spacing: 0.03em;
          color: #9aabbd;
          white-space: nowrap;
        }

        .wb-step-line {
          width: 22px;
          height: 1px;
          background: #e0e7ef;
          margin: 0 9px;
          flex: 0 0 auto;
        }

        .wb-step.is-current .wb-step-dot {
          background: #2b598e;
          border-color: #2b598e;
          color: #ffffff;
        }

        .wb-step.is-current .wb-step-name {
          color: #24466d;
        }

        .wb-step.is-done .wb-step-dot {
          background: #e3eef7;
          border-color: #cfe0ee;
          color: #3f7fa0;
        }

        .wb-step.is-done .wb-step-name {
          color: #6d8ba3;
        }

        /* ---- Body: sidebar + work area ---- */
        .wb-body {
          flex: 1 1 auto;
          display: flex;
          min-height: 0;
        }

        .wb-side {
          flex: 0 0 372px;
          width: 372px;
          display: flex;
          flex-direction: column;
          min-height: 0;
          background: #f9fbfd;
          border-right: 1px solid #e3eaf2;
        }

        .wb-side-scroll {
          flex: 1 1 auto;
          min-height: 0;
          overflow-y: auto;
          overflow-x: hidden;
        }

        .wb-section {
          padding: 20px 24px 22px 24px;
          border-bottom: 1px solid #e8eef5;
        }

        .wb-section:last-child {
          border-bottom: 0;
        }

        .wb-section-title {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0;
          font-size: 11.5px;
          font-weight: 700;
          letter-spacing: 0.085em;
          text-transform: uppercase;
          color: #5c7186;
        }

        .wb-section-title i {
          font-size: 12px;
          color: #7d93aa;
        }

        .wb-field-label {
          display: flex;
          align-items: center;
          gap: 7px;
          margin-bottom: 8px;
          font-size: 13.5px;
          font-weight: 600;
          color: #34485f;
        }

        .wb-hint {
          margin: 6px 0 0 0;
          font-size: 12.5px;
          line-height: 1.55;
          color: #7e90a3;
        }

        .wb-subfield {
          padding-top: 18px;
          border-top: 1px dashed #e1e9f2;
        }

        .wb-inline-actions {
          display: flex;
          gap: 8px;
          flex-wrap: wrap;
          margin-top: 18px;
        }

        /* ---- Sticky action bar ---- */
        .wb-actions {
          flex: 0 0 auto;
          display: flex;
          gap: 10px;
          padding: 14px 24px;
          background: #ffffff;
          border-top: 1px solid #e3eaf2;
        }

        .wb-actions .btn {
          flex: 1 1 0;
          font-weight: 600;
          font-size: 14px;
          padding: 10px 14px;
          border-radius: 8px;
        }

        .wb-actions .btn[disabled],
        .wb-actions .shiny-download-link[disabled] {
          pointer-events: none;
          color: #8da0b3 !important;
          background: #edf1f5 !important;
          border-color: #dce4ec !important;
          box-shadow: none !important;
          opacity: 1;
        }

        /* ---- Work area ---- */
        .wb-main {
          position: relative;
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
          min-width: 0;
          min-height: 0;
          background: #ffffff;
        }

        .wb-main-inner {
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        /* ---- Step-complete card ---- */
        /* ---- Stacked plot panels in a work area ---- */
        .wb-plot-stack {
          flex: 1 1 auto;
          min-height: 0;
          overflow-y: auto;
          overflow-x: hidden;
          padding: 20px 26px 24px 26px;
          display: flex;
          flex-direction: column;
          gap: 18px;
        }

        .wb-plot-card {
          flex: 0 0 auto;
          border: 1px solid #e6edf4;
          border-radius: 12px;
          background: #ffffff;
          padding: 14px 16px 10px 16px;
        }

        .wb-plot-title {
          font-size: 11.5px;
          font-weight: 700;
          letter-spacing: 0.07em;
          text-transform: uppercase;
          color: #7b8fa3;
          margin-bottom: 10px;
        }

        .wb-plot-card .empty-panel {
          min-height: 260px;
          border: 1px dashed #e2e9f1;
          background: #fbfcfe;
          color: #9aabbd;
          font-size: 13.5px;
        }

        .wb-plot-card .shiny-plotly-output,
        .wb-plot-card .plotly,
        .wb-plot-card .html-widget {
          width: 100% !important;
          max-width: 100% !important;
        }

        /* ---- Metric tiles: headline number, no chart ----
           Earlier versions gave two numbers a whole bar chart each, which
           spent a lot of area on very little information. */
        .wb-metric-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
          gap: 16px;
        }

        .wb-metric {
          padding: 22px 24px 24px 24px;
          border: 1px solid #e6edf4;
          border-radius: 12px;
          background: #ffffff;
        }

        .wb-metric-name {
          display: flex;
          align-items: center;
          gap: 8px;
          font-size: 13px;
          font-weight: 700;
          letter-spacing: 0.045em;
          text-transform: uppercase;
          color: #6d8299;
        }

        .wb-metric-figure {
          display: flex;
          align-items: baseline;
          flex-wrap: wrap;
          gap: 12px;
          margin: 16px 0 6px 0;
        }

        .wb-metric-value {
          font-size: 36px;
          font-weight: 700;
          line-height: 1;
          letter-spacing: -0.02em;
          color: #1c3450;
          font-variant-numeric: tabular-nums;
        }

        .wb-delta {
          display: inline-flex;
          align-items: center;
          gap: 5px;
          padding: 4px 10px;
          border-radius: 999px;
          font-size: 12px;
          font-weight: 700;
          font-variant-numeric: tabular-nums;
          white-space: nowrap;
        }

        .wb-delta.is-good {
          background: #e8f5f1;
          color: #227f6f;
        }

        .wb-delta.is-flat {
          background: #eef2f7;
          color: #7b8fa3;
        }

        .wb-delta.is-up {
          background: #fdf1e7;
          color: #a8672c;
        }

        .wb-metric-was {
          font-size: 12.5px;
          color: #9aabbd;
          font-variant-numeric: tabular-nums;
        }

        .wb-metric-was b {
          color: #7b8fa3;
          font-weight: 600;
        }

        .wb-metric-chart {
          margin-top: 20px;
        }

        .wb-metric-chart svg {
          display: block;
          width: 100%;
          height: auto;
        }

        .wb-metric-desc {
          margin: 14px 0 0 0;
          padding-top: 14px;
          border-top: 1px solid #f0f4f8;
          font-size: 12.5px;
          line-height: 1.65;
          color: #8296ab;
        }

        .wb-metric-note {
          margin: 16px 0 0 0;
          font-size: 12.5px;
          color: #93a5b8;
        }

        /* ---- Empty work area: what to do on this page ---- */
        .wb-empty-layer {
          position: absolute;
          top: 49px;
          left: 0;
          right: 0;
          bottom: 0;
          z-index: 5;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 24px;
          background: #ffffff;
        }

        .wb-empty {
          width: 100%;
          max-width: 840px;
          text-align: center;
        }

        .wb-empty-icon {
          width: 62px;
          height: 62px;
          margin: 0 auto 18px auto;
          border-radius: 16px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 24px;
          color: #4a7bb0;
          background: linear-gradient(160deg, #eef4fb 0%, #e6f1f4 100%);
          border: 1px solid #e0eaf3;
        }

        .wb-empty-title {
          font-size: 18px;
          font-weight: 700;
          color: #24405e;
          margin-bottom: 20px;
        }

        .wb-empty-steps {
          text-align: left;
          border: 1px solid #e6edf4;
          border-radius: 12px;
          overflow: hidden;
          background: #fbfcfe;
        }

        .wb-empty-step {
          display: flex;
          align-items: center;
          gap: 12px;
          padding: 13px 20px;
          border-bottom: 1px solid #eef2f7;
        }

        .wb-empty-step:last-child {
          border-bottom: 0;
        }

        .wb-empty-step-num {
          flex: 0 0 auto;
          width: 21px;
          height: 21px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 11px;
          font-weight: 700;
          background: #e3ecf7;
          color: #2b598e;
        }

        .wb-empty-step-text {
          font-size: 13.5px;
          line-height: 1.5;
          color: #4a627c;
        }

        .wb-empty-step-text b {
          color: #26415e;
          font-weight: 600;
        }

        .wb-empty-tag {
          display: inline-block;
          margin-left: 6px;
          padding: 1px 7px;
          border-radius: 999px;
          font-size: 10.5px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          vertical-align: 1px;
        }

        .wb-empty-tag.is-required {
          background: #fdeeee;
          color: #b4544f;
        }

        .wb-empty-tag.is-optional {
          background: #eef2f7;
          color: #7b8fa3;
        }

        .wb-empty-foot {
          margin-top: 16px;
          font-size: 12.5px;
          color: #93a5b8;
        }

        .wb-empty-foot b {
          color: #4a627c;
          font-weight: 600;
        }

        /* ---- Success state inside the process card ---- */
        .fm-modal-actions {
          margin-top: 22px;
          display: flex;
          justify-content: center;
        }

        .fm-modal-actions .btn {
          min-width: 132px;
          font-weight: 600;
          padding: 9px 20px;
          border-radius: 8px;
        }

        .wb-check {
          width: 84px;
          height: 84px;
          margin: 0 auto 18px auto;
          display: block;
        }

        .wb-check-ring {
          fill: none;
          stroke: #2f9e8f;
          stroke-width: 2.6;
          stroke-linecap: round;
          stroke-dasharray: 151;
          stroke-dashoffset: 151;
          transform-origin: 50% 50%;
          transform: rotate(-90deg);
          animation: wbDraw 0.62s cubic-bezier(0.65, 0, 0.35, 1) 0.1s forwards;
        }

        .wb-check-mark {
          fill: none;
          stroke: #2f9e8f;
          stroke-width: 3.4;
          stroke-linecap: round;
          stroke-linejoin: round;
          stroke-dasharray: 40;
          stroke-dashoffset: 40;
          animation: wbDraw 0.34s cubic-bezier(0.65, 0, 0.35, 1) 0.62s forwards;
        }

        .wb-check-halo {
          fill: #2f9e8f;
          opacity: 0;
          transform-origin: 50% 50%;
          animation: wbHalo 0.75s ease-out 0.62s forwards;
        }

        @keyframes wbDraw {
          to { stroke-dashoffset: 0; }
        }

        @keyframes wbHalo {
          0%   { opacity: 0.20; transform: scale(0.55); }
          100% { opacity: 0;    transform: scale(1.20); }
        }

        .wb-done-rows {
          display: flex;
          flex-direction: column;
          gap: 1px;
          margin: 22px auto 0 auto;
          max-width: 420px;
          background: #eaf0f6;
          border: 1px solid #e6edf4;
          border-radius: 10px;
          overflow: hidden;
          text-align: left;
        }

        .wb-done-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 14px;
          padding: 11px 15px;
          background: #fbfcfe;
          font-size: 13px;
        }

        .wb-done-row-label {
          display: flex;
          align-items: center;
          gap: 9px;
          color: #5c7186;
          font-weight: 500;
        }

        .wb-done-row-label i {
          color: #90a4b8;
          font-size: 12px;
        }

        .wb-done-row-value {
          font-weight: 700;
          color: #26415e;
          font-variant-numeric: tabular-nums;
          white-space: nowrap;
        }

        /* ---- Empty work area: what to do on this page ---- */
        .wb-empty-layer {
          position: absolute;
          top: 49px;
          left: 0;
          right: 0;
          bottom: 0;
          z-index: 5;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 24px;
          background: #ffffff;
        }

        .wb-empty {
          width: 100%;
          max-width: 840px;
          text-align: center;
        }

        /* work areas without a tab strip (function page): cover the whole area */
        #function_result_card .wb-empty-layer {
          top: 0;
        }

        .wb-empty-icon {
          width: 62px;
          height: 62px;
          margin: 0 auto 18px auto;
          border-radius: 16px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 24px;
          color: #4a7bb0;
          background: linear-gradient(160deg, #eef4fb 0%, #e6f1f4 100%);
          border: 1px solid #e0eaf3;
        }

        .wb-empty-title {
          font-size: 18px;
          font-weight: 700;
          color: #24405e;
          margin-bottom: 20px;
        }

        .wb-empty-steps {
          text-align: left;
          border: 1px solid #e6edf4;
          border-radius: 12px;
          overflow: hidden;
          background: #fbfcfe;
        }

        .wb-empty-step {
          display: flex;
          align-items: center;
          gap: 12px;
          padding: 13px 20px;
          border-bottom: 1px solid #eef2f7;
        }

        .wb-empty-step:last-child {
          border-bottom: 0;
        }

        .wb-empty-step-num {
          flex: 0 0 auto;
          width: 21px;
          height: 21px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 11px;
          font-weight: 700;
          background: #e3ecf7;
          color: #2b598e;
        }

        .wb-empty-step-text {
          font-size: 13.5px;
          line-height: 1.5;
          color: #4a627c;
        }

        .wb-empty-step-text b {
          color: #26415e;
          font-weight: 600;
        }

        .wb-empty-tag {
          display: inline-block;
          margin-left: 6px;
          padding: 1px 7px;
          border-radius: 999px;
          font-size: 10.5px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          vertical-align: 1px;
        }

        .wb-empty-tag.is-required {
          background: #fdeeee;
          color: #b4544f;
        }

        .wb-empty-tag.is-optional {
          background: #eef2f7;
          color: #7b8fa3;
        }

        .wb-empty-foot {
          margin-top: 16px;
          font-size: 12.5px;
          color: #93a5b8;
        }

        .wb-empty-foot b {
          color: #4a627c;
          font-weight: 600;
        }

        .wb-toast-slot {
          display: contents;
        }

        /* Top-centre of the whole window, like the quick-start dialog. */





        /* animated tick */
        .wb-check {
          width: 76px;
          height: 76px;
          margin: 0 auto 16px auto;
          display: block;
        }

        .wb-check-ring {
          fill: none;
          stroke: #2f9e8f;
          stroke-width: 2.6;
          stroke-linecap: round;
          stroke-dasharray: 151;
          stroke-dashoffset: 151;
          transform-origin: 50% 50%;
          transform: rotate(-90deg);
          animation: wbDraw 0.62s cubic-bezier(0.65, 0, 0.35, 1) 0.1s forwards;
        }

        .wb-check-mark {
          fill: none;
          stroke: #2f9e8f;
          stroke-width: 3.4;
          stroke-linecap: round;
          stroke-linejoin: round;
          stroke-dasharray: 40;
          stroke-dashoffset: 40;
          animation: wbDraw 0.34s cubic-bezier(0.65, 0, 0.35, 1) 0.62s forwards;
        }

        .wb-check-halo {
          fill: #2f9e8f;
          opacity: 0;
          transform-origin: 50% 50%;
          animation: wbHalo 0.75s ease-out 0.62s forwards;
        }

        @keyframes wbDraw {
          to { stroke-dashoffset: 0; }
        }

        @keyframes wbHalo {
          0%   { opacity: 0.20; transform: scale(0.55); }
          100% { opacity: 0;    transform: scale(1.20); }
        }



        .wb-done-rows {
          display: flex;
          flex-direction: column;
          gap: 1px;
          background: #eaf0f6;
          border: 1px solid #e6edf4;
          border-radius: 10px;
          overflow: hidden;
          text-align: left;
        }

        .wb-done-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 14px;
          padding: 11px 15px;
          background: #fbfcfe;
          font-size: 13px;
        }

        .wb-done-row-label {
          display: flex;
          align-items: center;
          gap: 9px;
          color: #5c7186;
          font-weight: 500;
        }

        .wb-done-row-label i {
          color: #90a4b8;
          font-size: 12px;
        }

        .wb-done-row-value {
          font-weight: 700;
          color: #26415e;
          font-variant-numeric: tabular-nums;
          white-space: nowrap;
        }

        .wb-done-row.is-muted .wb-done-row-value {
          color: #a9b8c8;
          font-weight: 600;
        }

        /* ---- Buttons ---- */
        .btn-wb-primary,
        .btn.btn-wb-primary {
          background: #2b598e;
          border: 1px solid #2b598e;
          color: #ffffff;
        }

        .btn-wb-primary:hover,
        .btn.btn-wb-primary:hover {
          background: #234b7b;
          border-color: #234b7b;
          color: #ffffff;
        }

        .btn-wb-next,
        .btn.btn-wb-next {
          background: #ffffff;
          border: 1px solid #cbd8e6;
          color: #2b598e;
        }

        .btn-wb-next:hover,
        .btn.btn-wb-next:hover {
          background: #f1f6fb;
          border-color: #9fb8d2;
          color: #1d4a7a;
        }

        .btn-wb-next[disabled],
        .btn.btn-wb-next[disabled] {
          color: #a9b8c8;
          border-color: #e0e7ef;
          background: #f7f9fc;
        }

        .btn-wb-ghost,
        .btn.btn-wb-ghost {
          background: #ffffff;
          border: 1px solid #d9e2ec;
          color: #4a627c;
          font-size: 13px;
          font-weight: 600;
          padding: 7px 12px;
          border-radius: 7px;
        }

        .btn-wb-ghost:hover,
        .btn.btn-wb-ghost:hover {
          background: #f1f6fb;
          border-color: #b9cbdf;
          color: #2b598e;
        }

        /* ---- Form controls inside the sidebar ---- */
        /* Spacing comes from flex `gap`, not from margins on the controls.
           Margin-based rules kept failing here because a conditionalPanel is
           a plain <div data-display-if> whose margins Shiny and Bootstrap
           both touch; `gap` applies between every pair of children whatever
           they are, and a hidden panel contributes no gap at all. */
        .wb-side .wb-section,
        .wb-side .wb-subfield {
          display: flex;
          flex-direction: column;
          gap: 18px;
        }

        .wb-side [data-display-if][style*='display: none'] {
          display: none !important;
        }

        .wb-side .form-group,
        .wb-side .shiny-input-container,
        .wb-side .wb-section > *,
        .wb-side .wb-subfield > *,
        .wb-side [data-display-if] > * {
          margin-top: 0;
          margin-bottom: 0;
        }

        /* the gap already separates the title from the first control */
        .wb-side .wb-section-title {
          margin-bottom: 0;
        }

        /* An unrendered uiOutput is still a flex child, so it would add a
           second gap and push the next control down. */
        .wb-side .shiny-html-output:empty,
        .wb-side [data-display-if]:empty {
          display: none;
        }

        /* checkboxes read as one line, so they need less air above */
        .wb-side .checkbox,
        .wb-side .form-group.shiny-input-checkboxgroup {
          margin: 0;
        }

        .wb-side .checkbox label {
          display: flex;
          align-items: center;
          gap: 8px;
          font-size: 13.5px;
          color: #3d556f;
          font-weight: 500;
          padding-left: 0;
        }

        .wb-side .checkbox input[type='checkbox'],
        .wb-side .radio input[type='radio'] {
          position: static;
          margin: 0;
          flex: 0 0 auto;
        }

        .wb-side .radio {
          margin: 0;
        }

        .wb-side .radio + .radio {
          margin-top: 9px;
        }

        .wb-side .radio label {
          display: flex;
          align-items: center;
          gap: 8px;
          padding-left: 0;
        }

        /* fluidRow of small numeric inputs (grid tuning) */
        .wb-side .row {
          margin-left: -6px;
          margin-right: -6px;
        }

        .wb-side .row > [class*='col-'] {
          padding-left: 6px;
          padding-right: 6px;
        }

        .wb-side .row > [class*='col-'] .form-group {
          margin-top: 0;
        }

        .wb-side .row + .row {
          margin-top: 14px;
        }

        .wb-side .control-label,
        .wb-side .shiny-input-container > label {
          display: block;
          font-size: 13.5px;
          font-weight: 600;
          color: #34485f;
          margin-bottom: 9px;
          line-height: 1.35;
        }

        .wb-side .btn-file {
          background: #edf2f8;
          border: 1px solid #d5e0ec;
          border-right: 0;
          color: #35577f;
          font-weight: 600;
          font-size: 13px;
        }

        .wb-side .btn-file:hover {
          background: #e2ebf5;
          color: #23446b;
        }

        .wb-side .input-group .form-control.wb-file-demo {
          color: #26415e;
          font-weight: 600;
        }

        .wb-side .input-group .form-control {
          font-size: 13px;
          color: #7b8c9e;
          background: #ffffff;
          border-color: #d5e0ec;
          box-shadow: none;
        }

        /* Sliders (ionRangeSlider). The stock widget stacks value bubble,
           min/max captions, track and a tick grid into ~40px, which in a
           372px sidebar reads as one squashed block. Only the value bubble
           and the track are kept, and they get real vertical separation. */
        .wb-side .irs {
          font-family: inherit;
        }

        .wb-side .irs--shiny {
          height: 58px;
          margin-top: 2px;
        }

        /* tick grid and the 0 / 1 end captions collide with the bubble */
        .wb-side .irs--shiny .irs-grid,
        .wb-side .irs--shiny .irs-min,
        .wb-side .irs--shiny .irs-max {
          display: none;
        }

        .wb-side .irs--shiny .irs-single {
          top: 0;
          background: #2b598e;
          color: #ffffff;
          font-size: 11.5px;
          font-weight: 700;
          line-height: 1.35;
          padding: 3px 9px;
          border-radius: 6px;
        }

        .wb-side .irs--shiny .irs-single:before {
          border-top-color: #2b598e;
        }

        .wb-side .irs--shiny .irs-line {
          height: 6px;
          top: 36px;
          background: #e4ebf3;
          border: 0;
          border-radius: 999px;
        }

        .wb-side .irs--shiny .irs-bar {
          height: 6px;
          top: 36px;
          background: #2b598e;
          border: 0;
          border-radius: 999px;
        }

        .wb-side .irs--shiny .irs-handle {
          top: 30px;
          width: 18px;
          height: 18px;
          border: 2px solid #2b598e;
          background: #ffffff;
          box-shadow: 0 1px 3px rgba(28, 52, 80, 0.22);
        }

        .wb-side .irs--shiny .irs-handle > i {
          display: none;
        }

        .wb-side .progress {
          height: 4px;
          margin-top: 8px;
          margin-bottom: 0;
          background: #e8eef5;
        }

        .wb-side .radio label,
        .wb-side .shiny-options-group label {
          font-size: 13.5px;
          color: #3d556f;
          font-weight: 500;
        }

        /* ---- Preview surface ---- */
        .workbench .wb-preview-shell {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
          flex-direction: column;
        }

        .workbench .wb-preview-shell > .tabbable {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
          flex-direction: column;
        }

        .workbench .wb-preview-shell > .tabbable > .nav-tabs {
          flex: 0 0 auto;
          margin: 0;
          padding: 0 26px;
          gap: 0;
          border-bottom: 1px solid #e8eef5;
          background: #ffffff;
        }

        .workbench .nav-tabs .nav-link {
          border: 0;
          border-bottom: 2px solid transparent;
          border-radius: 0;
          background: transparent;
          padding: 14px 2px;
          margin-right: 26px;
          font-size: 13.5px;
          font-weight: 600;
          color: #71869b;
        }

        .workbench .nav-tabs .nav-link:hover {
          color: #2b598e;
          border-bottom-color: #c8d8e9;
        }

        .workbench .nav-tabs .nav-link.active {
          color: #1d4a7a;
          background: transparent;
          border-bottom-color: #2b598e;
        }

        .workbench .wb-preview-shell > .tabbable > .tab-content {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
        }

        .workbench .wb-preview-shell > .tabbable > .tab-content > .tab-pane {
          flex: 1 1 auto;
          min-height: 0;
          min-width: 0;
        }

        .workbench .wb-preview-shell > .tabbable > .tab-content > .active {
          display: flex;
          flex-direction: column;
          min-height: 0;
          overflow: hidden;
        }

        .workbench .wb-preview-table {
          flex: 1 1 auto;
          min-height: 0;
          height: 100%;
          display: flex;
          flex-direction: column;
          overflow: hidden;
          padding: 0;
        }

        .workbench .wb-preview-table > .shiny-html-output,
        .workbench .wb-preview-table > .datatables {
          flex: 1 1 auto;
          min-height: 0;
          height: 100%;
          display: flex;
          flex-direction: column;
        }

        /* =========== DataTables chrome =========== */
        .workbench .dataTables_wrapper {
          flex: 1 1 auto;
          min-height: 0;
          display: flex;
          flex-direction: column;
          width: 100%;
        }

        .workbench .wb-dt-scroll {
          flex: 1 1 auto;
          min-height: 0;
          height: 100%;
          overflow: auto;
        }

        .workbench .wb-dt-bottom {
          flex: 0 0 auto;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 18px;
          padding: 11px 26px;
          border-top: 1px solid #eef2f7;
          background: #fcfdfe;
        }

        /* rows-per-page */
        .workbench .dataTables_length {
          margin: 0;
        }

        .workbench .dataTables_length label {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0;
          font-size: 12.5px;
          font-weight: 500;
          color: #7b8fa3;
        }

        .workbench .dataTables_length select {
          border: 1px solid #d7e0ea;
          border-radius: 7px;
          padding: 5px 26px 5px 9px;
          font-size: 12.5px;
          font-weight: 600;
          color: #34485f;
          background-color: #ffffff;
          height: auto;
        }

        /* pagination: Prev | page jump | Next */
        .workbench .dataTables_paginate {
          margin: 0 !important;
          padding: 0 !important;
        }

        .workbench .dataTables_paginate ul.pagination {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0;
          padding: 0;
        }

        .workbench .dataTables_paginate .paginate_button,
        .workbench .dataTables_paginate .page-item {
          margin: 0 !important;
          border: 0 !important;
          background: transparent !important;
          box-shadow: none !important;
        }

        .workbench .dataTables_paginate .page-link,
        .workbench .dataTables_paginate a.paginate_button {
          display: inline-block;
          margin: 0 !important;
          padding: 6px 14px !important;
          border: 1px solid #d7e0ea !important;
          border-radius: 7px !important;
          background: #ffffff !important;
          box-shadow: none !important;
          font-size: 12.5px;
          font-weight: 600;
          color: #4a627c !important;
          cursor: pointer;
        }

        .workbench .dataTables_paginate .page-link:hover,
        .workbench .dataTables_paginate a.paginate_button:hover {
          background: #eef4fb !important;
          border-color: #9fb8d2 !important;
          color: #2b598e !important;
        }

        .workbench .dataTables_paginate .disabled .page-link,
        .workbench .dataTables_paginate .disabled .page-link:hover,
        .workbench .dataTables_paginate a.paginate_button.disabled,
        .workbench .dataTables_paginate a.paginate_button.disabled:hover {
          background: #f7f9fc !important;
          border-color: #e6ecf3 !important;
          color: #b6c2cf !important;
          cursor: default;
        }

        /* injected page-jump control */
        .workbench .wb-page-jump {
          display: flex !important;
          align-items: center;
          gap: 7px;
          margin: 0 2px;
          font-size: 12.5px;
          color: #7b8fa3;
          white-space: nowrap;
        }

        .workbench .wb-page-input {
          width: 58px;
          padding: 6px 8px;
          border: 1px solid #d7e0ea;
          border-radius: 7px;
          background: #ffffff;
          font-size: 12.5px;
          font-weight: 700;
          color: #26415e;
          text-align: center;
          font-variant-numeric: tabular-nums;
        }

        .workbench .wb-page-input:focus {
          outline: none;
          border-color: #9fb8d2;
          box-shadow: 0 0 0 3px rgba(43, 89, 142, 0.10);
        }

        .workbench .wb-page-total {
          font-weight: 600;
          color: #8296ab;
          font-variant-numeric: tabular-nums;
        }

        .workbench .dataTables_info {
          display: none;
        }

        /* =========== The table itself =========== */
        .workbench table.dataTable {
          width: 100% !important;
          min-width: 100%;
          margin: 0 !important;
          border-collapse: separate;
          border-spacing: 0;
        }

        .workbench table.dataTable thead th,
        .workbench table.dataTable tbody td {
          white-space: nowrap;
        }

        .workbench table.dataTable thead th {
          position: sticky;
          top: 0;
          z-index: 2;
          background: #f5f8fb;
          border-top: 0;
          border-bottom: 1px solid #e1e9f1;
          padding: 11px 18px;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.01em;
          color: #64798f;
          white-space: nowrap;
        }

        .workbench table.dataTable tbody td {
          padding: 10px 18px;
          font-size: 13.5px;
          line-height: 1.45;
          color: #3a4f66;
          border-top: 0;
          border-bottom: 1px solid #f1f5f9;
          background: transparent;
        }

        .workbench table.dataTable tbody tr:hover td {
          background: #f6fafd;
        }

        .workbench table.dataTable tbody tr:last-child td {
          border-bottom: 0;
        }

        /* empty-state table */
        .workbench table.dataTable.wb-dt-empty thead {
          display: none;
        }

        .workbench table.dataTable.wb-dt-empty tbody td {
          padding: 46px 26px;
          text-align: center;
          color: #9aabbd;
          font-size: 13.5px;
          border-bottom: 0;
        }

        .workbench table.dataTable.wb-dt-empty tbody tr:hover td {
          background: transparent;
        }

        .workbench #module_network_ui {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
          border: 1px solid #e6edf4;
          border-radius: 10px;
          overflow: hidden;
        }

        /* a plain scrolling work area (function / results pages) */
        .wb-scroll-area {
          flex: 1 1 auto;
          min-height: 0;
          overflow-y: auto;
          overflow-x: hidden;
          padding: 22px 28px 26px 28px;
        }

        /* ---- Narrow screens: stack the two regions ---- */
        @media (max-width: 991.98px) {
          .workbench {
            height: auto;
          }

          .wb-body {
            flex-direction: column;
          }

          .wb-side {
            flex: 0 0 auto;
            width: 100%;
            border-right: 0;
            border-bottom: 1px solid #e3eaf2;
          }

          .wb-side-scroll {
            overflow: visible;
          }

          .wb-stepper {
            display: none;
          }

          .workbench .wb-preview-table {
            min-height: 320px;
          }
        }

        @media (max-width: 1400px) {
          .wb-step-name {
            display: none;
          }

          .wb-step-line {
            width: 16px;
            margin: 0 6px;
          }
        }

        /* Modules */



        
        
        
        





        #modules_page .module-visual-shell,
        #modules_page .module-table-shell {
          display: flex;
          flex-direction: column;
          flex: 1 1 auto;
          min-height: 0;
          padding: 16px 26px 20px 26px;
        }

        #modules_page .module-visual-filter-row {
          display: flex;
          align-items: center;
          justify-content: flex-start;
          gap: 12px;
          flex-wrap: nowrap;
          margin-bottom: 8px;
          width: 100%;
        }

        #modules_page .module-visual-filter-row #module_selector_ui {
          flex: 0 1 200px;
          width: 200px;
          max-width: 100%;
          min-width: 0;
          display: block;
        }

        #modules_page .module-visual-filter-row #module_node_selector_ui {
          flex: 0 1 320px;
          width: 320px;
          max-width: 100%;
          min-width: 0;
          display: block;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .pcsf-selector-item,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-item {
          width: 100%;
          max-width: 100%;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .pcsf-selector-input,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-input {
          display: flex;
          align-items: flex-start;
          gap: 8px;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .pcsf-selector-field,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-field {
          flex: 1;
          min-width: 0;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .pcsf-selector-field .form-group,
        #modules_page .module-visual-filter-row #module_selector_ui .pcsf-selector-field .selectize-control,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-field .form-group,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-field .selectize-control {
          margin-bottom: 0;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .pcsf-selector-input .inline-info-wrap,
        #modules_page .module-visual-filter-row #module_node_selector_ui .pcsf-selector-input .inline-info-wrap {
          margin-left: 0;
          margin-top: 8px;
          flex: 0 0 auto;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .selectize-input,
        #modules_page .module-visual-filter-row #module_node_selector_ui .selectize-input {
          height: 38px;
          min-height: 38px;
          max-height: 38px;
          overflow: hidden;
          display: flex;
          align-items: center;
          flex-wrap: nowrap;
        }

        #modules_page .module-visual-filter-row #module_selector_ui .selectize-input > .item,
        #modules_page .module-visual-filter-row #module_node_selector_ui .selectize-input > .item {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 100%;
        }

        #modules_page .module-visual-filter-row #module_network_legend {
          flex: 1 1 260px;
          margin-left: auto;
          min-width: 260px;
          display: flex;
          justify-content: flex-end;
          padding-right: 12px;
        }

        #modules_page .module-visual-filter-row #module_network_legend .network-legend-row {
          justify-content: flex-end;
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
        
        /* framed canvas, matching the PCSF subnetwork stage */
        #modules_page #module_network_ui {
          display: flex;
          flex: 1;
          min-height: 460px;
          width: 100%;
          border: 1px solid #e6edf4;
          border-radius: 10px;
          overflow: hidden;
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
        padding: 15px 28px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 0;
        position: relative;
        z-index: 1050;
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

      /* ---- Analysis dropdown ---- */
      .nav-pills .nav-item.nav-dropdown {
        position: relative;
      }

      .nav-dropdown-toggle {
        display: flex;
        align-items: center;
        gap: 7px;
        cursor: pointer;
        text-decoration: none !important;
      }

      .nav-caret {
        font-size: 10px;
        opacity: 0.7;
        transition: transform 0.2s ease;
      }

      .nav-dropdown.open .nav-caret {
        transform: rotate(180deg);
      }

      .nav-dropdown.open > .nav-link {
        background-color: rgba(255,255,255,0.2);
        color: #ffffff;
      }

      .nav-dropdown-menu {
        position: absolute;
        top: calc(100% + 12px);
        left: 50%;
        transform: translateX(-50%) translateY(-6px);
        width: max-content;
        min-width: 0;
        margin: 0;
        padding: 6px;
        list-style: none;
        background: #ffffff;
        border: 1px solid #dde5ef;
        border-radius: 12px;
        box-shadow: 0 18px 38px rgba(20, 42, 68, 0.20);
        opacity: 0;
        visibility: hidden;
        pointer-events: none;
        transition: opacity 0.18s ease, transform 0.18s ease, visibility 0.18s;
        z-index: 1080;
      }

      .nav-dropdown.open .nav-dropdown-menu {
        opacity: 1;
        visibility: visible;
        pointer-events: auto;
        transform: translateX(-50%) translateY(0);
      }

      .nav-dropdown-menu::before {
        content: \"\";
        position: absolute;
        top: -6px;
        left: 50%;
        margin-left: -6px;
        width: 11px;
        height: 11px;
        background: #ffffff;
        border-left: 1px solid #dde5ef;
        border-top: 1px solid #dde5ef;
        transform: rotate(45deg);
        border-radius: 2px 0 0 0;
      }

      .nav-dropdown-menu li {
        list-style: none;
      }

      a.nav-dropdown-item {
        display: flex !important;
        flex-wrap: nowrap;
        align-items: center;
        gap: 10px;
        padding: 8px 14px 8px 10px;
        border-radius: 8px;
        color: #33475f !important;
        font-size: 14px;
        font-weight: 500;
        line-height: 1.2;
        white-space: nowrap;
        text-decoration: none !important;
        transition: background-color 0.16s ease, color 0.16s ease;
      }

      a.nav-dropdown-item:hover {
        background-color: #eef4fb;
        color: #1d4a7a !important;
      }

      a.nav-dropdown-item.active {
        background-color: #e4eef9;
        color: #1d4a7a !important;
        font-weight: 600;
      }

      .nav-step-num {
        flex: 0 0 auto;
        width: 21px;
        height: 21px;
        border-radius: 50%;
        background: #e3ecf7;
        color: #2b598e;
        font-size: 11.5px;
        font-weight: 700;
        line-height: 1;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        vertical-align: middle;
        transition: background-color 0.16s ease, color 0.16s ease;
      }

      .nav-step-label {
        display: inline-block;
        vertical-align: middle;
        white-space: nowrap;
      }

      a.nav-dropdown-item:hover .nav-step-num,
      a.nav-dropdown-item.active .nav-step-num {
        background: #2b598e;
        color: #ffffff;
      }

      /* ---- Sign In pill ---- */
      .nav-pills .nav-link.nav-link-signin {
        border: 1px solid rgba(255,255,255,0.45);
        margin-left: 10px;
      }

      .nav-pills .nav-link.nav-link-signin:hover {
        background-color: rgba(255,255,255,0.18);
        border-color: rgba(255,255,255,0.7);
      }

      .nav-auth-item {
        display: flex;
        align-items: center;
      }

      .nav-auth-item .shiny-html-output {
        display: inline-flex;
        align-items: center;
        gap: 8px;
      }

      .nav-auth-user {
        max-width: 150px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }

      .nav-auth-logout {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 34px;
        height: 34px;
        border-radius: 8px;
        color: rgba(255, 255, 255, 0.88) !important;
        text-decoration: none !important;
        transition: background-color 0.16s ease, color 0.16s ease;
      }

      .nav-auth-logout:hover,
      .nav-auth-logout:focus {
        background-color: rgba(255, 255, 255, 0.16);
        color: #ffffff !important;
      }
      
      /* Home page redesign */
      #home_page {
        background:
          radial-gradient(920px 380px at 10% -10%, rgba(92, 142, 209, 0.16), transparent 65%),
          radial-gradient(840px 360px at 96% 0%, rgba(73, 170, 162, 0.14), transparent 62%),
          #f4f7fb;
        padding: 0;
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
        padding: 28px 18px 26px 18px;
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
        font-size: 40px;
        line-height: 1.1;
        font-weight: 700;
        color: #224d7b;
        letter-spacing: -0.01em;
        font-variant-numeric: tabular-nums;
        margin-bottom: 6px;
      }

      #home_page .feature-title {
        font-size: 18px;
        font-weight: 600;
        color: #728aa2;
        letter-spacing: 0.01em;
        margin-bottom: 0;
      }


      #home_page .workflow-section {
        background: transparent;
        padding: 30px 0 32px 0;
        margin: 46px auto 0 auto;
        width: calc(100% - 96px);
        max-width: 1520px;
        border-top: 1px solid #d3dfeb;
        border-bottom: 1px solid #d3dfeb;
      }

      #home_page .workflow-section .section-title {
        font-size: 38px;
        line-height: 1.15;
      }

      #home_page .workflow-panel {
        width: 100%;
        max-width: none;
        margin: 0;
        display: grid;
        grid-template-columns: minmax(360px, 0.44fr) minmax(520px, 0.56fr);
        gap: 0;
        align-items: stretch;
      }

      #home_page .workflow-copy {
        padding: 18px 28px 18px 0;
        border-right: none;
        background: transparent;
        display: flex;
        align-items: center;
      }

      #home_page .workflow-copy-grid {
        display: block;
        max-width: 620px;
      }

      #home_page .workflow-kicker {
        margin: 0 0 8px 0;
        color: #3c7898;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }

      #home_page .workflow-lede-title {
        margin: 0;
        color: #213d58;
        font-size: 32px;
        font-weight: 750;
        line-height: 1.25;
      }

      #home_page .workflow-lede {
        margin: 0;
        color: #41566b;
        font-size: 16px;
        line-height: 1.52;
        max-width: none;
      }

      #home_page .workflow-copy-grid > div + div {
        margin-top: 18px;
      }

      #home_page .workflow-lede + .workflow-lede {
        margin-top: 10px;
      }

      #home_page .workflow-credit {
        margin: 18px 0 0 0;
        padding: 16px 18px;
        border: 1px solid #cfdce8;
        border-left: 4px solid #2f7894;
        border-radius: 10px;
        background: rgba(255, 255, 255, 0.56);
        color: #52687e;
        font-size: 14px;
        line-height: 1.55;
      }

      #home_page .workflow-credit strong {
        display: block;
        color: #25496b;
        font-size: 15px;
        margin-bottom: 5px;
      }

      #home_page .workflow-figure {
        padding: 12px 0 12px 28px;
        border-left: 1px solid #d3dfeb;
        background: transparent;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 0;
      }

      #home_page .workflow-image {
        display: block;
        width: 100%;
        max-width: 100%;
        max-height: 600px;
        height: auto;
        object-fit: contain;
      }

      .app-footer {
        margin-top: 0;
        border-top: 1px solid #d9e3ef;
        background: #eef2f8;
        border-radius: 0 0 18px 18px;
        padding: 28px 44px;
      }

      .app-footer-inner {
        display: grid;
        grid-template-columns: minmax(240px, 0.95fr) minmax(280px, 0.85fr) minmax(240px, 0.75fr);
        gap: 26px;
        align-items: center;
        width: min(1180px, 100%);
        margin: 0 auto;
      }

      .footer-logo {
        display: block;
        width: min(320px, 100%);
        height: auto;
      }

      .footer-links {
        display: grid;
        grid-template-columns: repeat(2, minmax(120px, 1fr));
        gap: 14px 28px;
        justify-self: center;
        width: min(320px, 100%);
      }

      .footer-link {
        color: #53617d;
        font-size: 15px;
        font-weight: 700;
        text-decoration: none;
        cursor: pointer;
      }

      .footer-link:hover,
      .footer-link:focus {
        color: #244d78;
        text-decoration: none;
      }

      .footer-meta {
        text-align: right;
        color: #778299;
        font-size: 14px;
        line-height: 1.55;
      }

      .footer-product {
        margin: 0 0 12px 0;
        color: #26324a;
        font-size: 18px;
        font-weight: 800;
      }

      .footer-meta p {
        margin: 0 0 8px 0;
      }

      .footer-meta strong {
        color: #53617d;
      }

      @media (max-width: 1080px) {
        #home_page .hero-section {
          min-height: 50vh;
        }

        #home_page .hero-subtitle {
          white-space: normal;
        }

        #home_page .workflow-panel {
          grid-template-columns: 1fr;
          gap: 18px;
        }

        #home_page .workflow-copy {
          padding-right: 0;
          padding-bottom: 22px;
        }

        #home_page .workflow-copy-grid {
          max-width: none;
        }

        #home_page .workflow-figure {
          min-height: 0;
          border-left: none;
          border-top: 1px solid #d3dfeb;
          padding-left: 0;
          padding-top: 22px;
        }

        .app-footer-inner {
          grid-template-columns: 1fr;
          gap: 22px;
        }

        .footer-meta {
          text-align: left;
        }
      }

      @media (max-width: 768px) {
        #home_page {
          padding: 0;
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

        #home_page .workflow-section .section-title {
          font-size: 30px;
        }

        #home_page .workflow-section {
          width: 100%;
        }

        #home_page .workflow-copy {
          padding: 18px 0 14px 0;
        }

        #home_page .workflow-copy-grid {
          max-width: none;
        }

        #home_page .workflow-lede-title {
          font-size: 23px;
        }

        #home_page .workflow-lede {
          font-size: 14px;
        }

        #home_page .workflow-credit {
          font-size: 13px;
        }

        #home_page .workflow-figure {
          padding: 18px 0 0 0;
        }

        #home_page .workflow-image {
          width: 100%;
          max-height: none;
        }

        .app-footer {
          padding: 26px 20px;
        }

        .footer-links {
          grid-template-columns: 1fr;
          gap: 12px;
        }
      }

      /* Authors page */
      #authors_page {
        background: #f4f7fb;
      }

      #authors_page .authors-page {
        min-height: calc(100vh - var(--fm-nav-h, 70px));
        overflow-x: hidden;
        color: #20364d;
      }

      #authors_page .authors-hero-section {
        position: relative;
        overflow: hidden;
        padding: 30px 40px 28px 40px;
        background:
          linear-gradient(90deg, rgba(255, 255, 255, 0.06) 1px, transparent 1px),
          linear-gradient(0deg, rgba(255, 255, 255, 0.05) 1px, transparent 1px),
          linear-gradient(126deg, #1e4a72 0%, #2d6e91 58%, #3e908e 100%);
        background-size: 52px 52px, 52px 52px, auto;
        border-bottom: 1px solid rgba(255, 255, 255, 0.28);
      }

      #authors_page .authors-hero-inner {
        width: min(1180px, 100%);
        margin: 0 auto;
        display: grid;
        grid-template-columns: minmax(0, 1fr) 260px;
        gap: 38px;
        align-items: center;
      }

      #authors_page .authors-kicker,
      #authors_page .authors-section-kicker {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        margin: 0;
        color: #2f7894;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0;
        text-transform: uppercase;
      }

      #authors_page .authors-kicker {
        color: #d8eef7;
      }

      #authors_page .authors-hero-title {
        margin: 12px 0 0 0;
        color: #ffffff;
        font-size: 34px;
        line-height: 1.08;
        font-weight: 800;
        letter-spacing: 0;
      }

      #authors_page .authors-hero-subtitle {
        margin: 9px 0 0 0;
        max-width: 620px;
        color: rgba(238, 248, 252, 0.82);
        font-size: 14px;
        line-height: 1.45;
        font-weight: 700;
        letter-spacing: 0;
      }

      #authors_page .authors-logo-block {
        justify-self: end;
        width: 260px;
        max-width: 100%;
        padding: 12px 0 0 0;
        border-top: 1px solid rgba(255, 255, 255, 0.36);
      }

      #authors_page .authors-logo-block img {
        display: block;
        width: 100%;
        height: auto;
        filter: drop-shadow(0 12px 20px rgba(5, 23, 40, 0.18));
      }

      #authors_page .authors-body {
        width: min(1180px, calc(100% - 80px));
        margin: 0 auto;
        padding: 42px 0 72px 0;
      }

      #authors_page .authors-section + .authors-section {
        margin-top: 54px;
      }

      #authors_page .authors-section-head {
        display: flex;
        align-items: flex-end;
        justify-content: space-between;
        gap: 24px;
        margin-bottom: 20px;
        padding-bottom: 16px;
        border-bottom: 1px solid #d4e1ed;
      }

      #authors_page .authors-section-title {
        margin: 6px 0 0 0;
        color: #213d58;
        font-size: 30px;
        line-height: 1.15;
        font-weight: 800;
        letter-spacing: 0;
      }

      #authors_page .faculty-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 22px;
      }

      #authors_page .student-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 18px;
      }

      #authors_page .author-card {
        min-width: 0;
        border: 1px solid #d7e2ec;
        border-top: 3px solid #3e908e;
        border-radius: 8px;
        background: #ffffff;
        box-shadow: 0 8px 20px rgba(41, 67, 95, 0.08);
        transition: transform 0.2s ease, box-shadow 0.2s ease, border-color 0.2s ease;
      }

      #authors_page .author-card:hover {
        transform: translateY(-2px);
        border-color: #bad0e2;
        box-shadow: 0 14px 26px rgba(41, 67, 95, 0.13);
      }

      #authors_page .faculty-author {
        display: grid;
        grid-template-columns: 168px minmax(0, 1fr);
        gap: 22px;
        align-items: center;
        padding: 22px;
      }

      #authors_page .student-author {
        padding: 18px;
      }

      #authors_page .author-photo {
        position: relative;
        overflow: hidden;
        border-radius: 8px;
        background: linear-gradient(135deg, #eaf4f8 0%, #dbe9f0 100%);
        border: 1px solid #dce8f1;
      }

      #authors_page .faculty-photo {
        width: 168px;
        aspect-ratio: 1;
      }

      #authors_page .student-photo {
        width: 100%;
        aspect-ratio: 1;
        margin-bottom: 16px;
      }

      #authors_page .author-photo img {
        display: block;
        width: 100%;
        height: 100%;
        object-fit: cover;
      }

      #authors_page .author-initials {
        width: 100%;
        height: 100%;
        display: none;
        place-items: center;
        color: #ffffff;
        background: linear-gradient(135deg, #2d6e91 0%, #3e908e 100%);
        font-size: 36px;
        line-height: 1;
        font-weight: 800;
        letter-spacing: 0;
      }

      #authors_page .student-photo .author-initials {
        font-size: 28px;
      }

      #authors_page .author-role-label {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        margin-bottom: 8px;
        color: #3e7895;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0;
        text-transform: uppercase;
      }

      #authors_page .author-name {
        margin: 0 0 8px 0;
        color: #172f47;
        font-size: 21px;
        line-height: 1.16;
        font-weight: 800;
        letter-spacing: 0;
      }

      #authors_page .student-author .author-name {
        font-size: 17px;
      }

      #authors_page .author-role {
        margin: 0 0 14px 0;
        color: #5b7188;
        font-size: 14px;
        line-height: 1.5;
        font-weight: 600;
      }

      #authors_page .student-author .author-role {
        font-size: 13px;
      }

      #authors_page .author-email {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        max-width: 100%;
        color: #2f7894;
        font-size: 13px;
        line-height: 1.35;
        font-weight: 800;
        text-decoration: none;
        overflow-wrap: anywhere;
      }

      #authors_page .author-email:hover,
      #authors_page .author-email:focus {
        color: #224d7b;
        text-decoration: none;
      }

      #authors_page .author-email span {
        min-width: 0;
        overflow-wrap: anywhere;
      }

      @media (prefers-reduced-motion: reduce) {
        #authors_page .author-card {
          transition: none !important;
        }

        #authors_page .author-card:hover {
          transform: none !important;
        }
      }

      @media (max-width: 1080px) {
        #authors_page .authors-hero-inner {
          grid-template-columns: 1fr;
          gap: 28px;
        }

        #authors_page .authors-logo-block {
          justify-self: start;
          width: 260px;
        }

        #authors_page .student-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 760px) {
        #authors_page .authors-hero-section {
          padding: 24px 20px 22px 20px;
        }

        #authors_page .authors-hero-title {
          font-size: 28px;
        }

        #authors_page .authors-hero-subtitle {
          font-size: 13px;
        }

        #authors_page .authors-body {
          width: calc(100% - 32px);
          padding: 30px 0 54px 0;
        }

        #authors_page .authors-section-head {
          display: block;
        }

        #authors_page .authors-section-title {
          font-size: 25px;
        }

        #authors_page .faculty-grid,
        #authors_page .student-grid {
          grid-template-columns: 1fr;
        }

        #authors_page .faculty-author,
        #authors_page .student-author {
          display: grid;
          grid-template-columns: 96px minmax(0, 1fr);
          gap: 16px;
          align-items: center;
          padding: 16px;
        }

        #authors_page .faculty-photo,
        #authors_page .student-photo {
          width: 96px;
          margin-bottom: 0;
        }

        #authors_page .author-name,
        #authors_page .student-author .author-name {
          font-size: 16px;
        }

        #authors_page .author-role,
        #authors_page .student-author .author-role {
          font-size: 12px;
        }

        #authors_page .author-email {
          font-size: 12px;
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

      /* =====================================================
         Function interpretation report
         Same language as the rest of the workbench: flat white
         surfaces, hairline borders, one blue accent, no drop shadows.
         ===================================================== */
      .fast-function-report {
        display: flex;
        flex-direction: column;
        gap: 16px;
      }

      .function-result-shell > .tabbable > .tab-content {
        padding-top: 26px;
      }

      /* ---- header bar: mode badge on the left, selectors on the right,
             so a single short dropdown no longer owns a whole row ---- */
      .fast-toolbar {
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 14px;
        margin-bottom: 16px;
      }

      .fast-toolbar-right {
        display: flex;
        align-items: center;
        flex-wrap: wrap;
        gap: 16px;
      }

      .fast-select {
        display: flex;
        align-items: center;
        gap: 9px;
      }

      .fast-select-label {
        font-size: 10.5px;
        font-weight: 700;
        letter-spacing: 0.07em;
        text-transform: uppercase;
        color: #93a5b8;
        white-space: nowrap;
      }

      .fast-select-field {
        width: 128px;
      }

      .fast-select-field.is-wide {
        width: 196px;
      }

      .fast-select-field .form-group,
      .fast-select-field .selectize-control {
        margin-bottom: 0;
      }

      .fast-select-field .selectize-input {
        display: flex;
        align-items: center;
        flex-wrap: nowrap;
        min-height: 34px;
        height: 34px;
        padding: 0 26px 0 12px;
        border-radius: 8px;
        border-color: #d7e0ea;
        font-size: 13px;
        overflow: hidden;
      }

      .fast-select-field .selectize-input > .item,
      .fast-select-field .selectize-input > input {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 100%;
      }

      /* ---- header block ---- */
      .fast-function-hero {
        position: relative;
        padding: 19px 22px;
        background: #f7fafd;
        border: 1px solid #e3ecf5;
        border-radius: 14px;
      }

      .fast-function-eyebrow {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        padding: 4px 11px;
        border-radius: 999px;
        background: #2b598e;
        border: 1px solid #2b598e;
        color: #ffffff;
        font-size: 10.5px;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        white-space: nowrap;
      }

      .fast-function-eyebrow i {
        font-size: 10px;
        color: #a8d8dd;
      }

      .fast-function-hero h3 {
        margin: 0 0 14px 0;
        color: #14304f;
        font-size: 23px;
        font-weight: 750;
        line-height: 1.34;
        letter-spacing: -0.015em;
      }

      .fast-function-meta {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
      }

      /* ---- pills and chips ---- */
      .fast-meta-pill,
      .fast-chip {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 5px 12px;
        border-radius: 999px;
        background: #ffffff;
        border: 1px solid #dbe6f2;
        color: #2b598e;
        font-size: 12px;
        font-weight: 600;
        line-height: 1.5;
        white-space: nowrap;
      }

      .fast-meta-pill i,
      .fast-chip i {
        font-size: 10.5px;
        color: #7ba8cd;
      }

      .fast-chip {
        white-space: normal;
        background: #eef5fb;
        border-color: #dbe8f4;
      }

      .fast-meta-pill.conf-high,
      .fast-chip.conf-high {
        background: #e4f4ee;
        border-color: #bfe3d5;
        color: #1c7a63;
      }

      .fast-meta-pill.conf-high i,
      .fast-chip.conf-high i { color: #3aa588; }

      .fast-meta-pill.conf-medium,
      .fast-chip.conf-medium {
        background: #fdf2e3;
        border-color: #f2ddc0;
        color: #9c6a22;
      }

      .fast-meta-pill.conf-medium i,
      .fast-chip.conf-medium i { color: #cf9a48; }

      .fast-meta-pill.conf-low,
      .fast-chip.conf-low {
        background: #fdecec;
        border-color: #f6d4d4;
        color: #ad4a45;
      }

      .fast-meta-pill.conf-low i,
      .fast-chip.conf-low i { color: #d47b76; }

      .fast-chip.dir-up {
        background: #fdecec;
        border-color: #f6d4d4;
        color: #ad4a45;
      }

      .fast-chip.dir-up i { color: #d47b76; }

      .fast-chip.dir-down {
        background: #e8f0fa;
        border-color: #cfe0f2;
        color: #1f4f86;
      }

      .fast-chip.dir-down i { color: #5b8fc4; }

      .fast-chip.dir-mixed {
        background: #f1f3f8;
        border-color: #e2e7ef;
        color: #66748c;
      }

      /* ---- sections, each with its own accent ---- */
      .fast-function-sections {
        display: flex;
        flex-direction: column;
        gap: 14px;
      }

      .fast-info-section {
        --accent: #2b598e;
        --accent-soft: #eef4fb;
        border: 1px solid #e8eef5;
        border-radius: 14px;
        background: #ffffff;
        overflow: hidden;
        box-shadow: 0 1px 2px rgba(16, 40, 70, 0.04),
                    0 8px 20px rgba(16, 40, 70, 0.045);
      }

      .fast-info-section.accent-blue   { --accent: #2b598e; --accent-soft: #eef4fb; }
      .fast-info-section.accent-teal   { --accent: #21867f; --accent-soft: #e9f5f3; }
      .fast-info-section.accent-violet { --accent: #6055b8; --accent-soft: #f0eefb; }
      .fast-info-section.accent-amber  { --accent: #b4712f; --accent-soft: #fdf3e7; }
      .fast-info-section.accent-green  { --accent: #2a8f6d; --accent-soft: #e8f5ef; }

      .fast-section-head {
        display: flex;
        align-items: center;
        gap: 11px;
        padding: 13px 18px;
        border-bottom: 1px solid #eef2f7;
      }

      .fast-section-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 28px;
        height: 28px;
        border-radius: 9px;
        background: var(--accent);
        color: #ffffff;
        font-size: 12px;
        flex: 0 0 auto;
      }

      .fast-section-head h4 {
        margin: 0;
        font-size: 13px;
        font-weight: 700;
        letter-spacing: -0.005em;
        color: #23405f;
      }

      .fast-section-body {
        padding: 6px 18px 18px 18px;
      }

      /* ---- the headline claim ---- */
      .fast-callout {
        margin: 12px 0 4px 0;
        padding: 15px 17px;
        border: 1px solid #e6edf4;
        border-radius: 11px;
        background: #fafcfe;
      }

      .fast-callout p {
        margin: 0;
        font-size: 14px;
        line-height: 1.75;
        color: #2c4762;
      }

      /* ---- label / value rows ---- */
      .fast-field {
        display: grid;
        grid-template-columns: 172px minmax(0, 1fr);
        gap: 18px;
        padding: 15px 0;
        border-bottom: 1px solid #f2f6fa;
      }

      .fast-field:last-child {
        border-bottom: 0;
        padding-bottom: 0;
      }

      .fast-field-label {
        font-size: 12.5px;
        font-weight: 600;
        color: #6d8299;
        line-height: 1.5;
        padding-top: 2px;
      }

      .fast-field-value {
        min-width: 0;
        font-size: 13.5px;
        line-height: 1.7;
        color: #35506d;
      }

      .fast-field-value p {
        margin: 0;
      }

      .fast-muted {
        font-size: 13px;
        color: #9fb0c2;
      }

      .fast-chip-list {
        display: flex;
        flex-wrap: wrap;
        gap: 7px;
      }

      /* ---- compact tables ---- */
      .fast-mini-table {
        width: 100%;
        border-collapse: separate;
        border-spacing: 0;
        border: 1px solid #e8eef5;
        border-radius: 11px;
        overflow: hidden;
      }

      .fast-mini-table th {
        padding: 10px 13px;
        background: var(--accent-soft);
        border-bottom: 1px solid #e8eef5;
        font-size: 10.5px;
        font-weight: 700;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        color: var(--accent);
        text-align: left;
        white-space: nowrap;
      }

      .fast-mini-table td {
        padding: 10px 13px;
        border-bottom: 1px solid #f2f6fa;
        font-size: 12.5px;
        line-height: 1.55;
        color: #35506d;
        vertical-align: top;
      }

      .fast-mini-table tr:last-child td {
        border-bottom: 0;
      }

      /* ---- pathway cards ---- */
      .fast-pathway-group + .fast-pathway-group {
        margin-top: 16px;
      }

      .fast-pathway-group-title {
        margin-bottom: 9px;
        font-size: 10.5px;
        font-weight: 700;
        letter-spacing: 0.07em;
        text-transform: uppercase;
        color: var(--accent);
      }

      .fast-pathway-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(248px, 1fr));
        gap: 9px;
      }

      .fast-pathway-card {
        display: flex;
        align-items: baseline;
        gap: 10px;
        padding: 10px 13px;
        border: 1px solid #e8eef5;
        border-radius: 11px;
        background: #fbfdfe;
        transition: border-color 0.16s ease, background-color 0.16s ease;
      }

      .fast-pathway-card:hover {
        border-color: #cfe0ee;
        background: var(--accent-soft);
      }

      .fast-pathway-id {
        flex: 0 0 auto;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 11px;
        font-weight: 700;
        color: var(--accent);
      }

      .fast-pathway-desc {
        min-width: 0;
        font-size: 12.5px;
        line-height: 1.55;
        color: #5c7186;
        overflow-wrap: anywhere;
      }

      /* ---- embedded transcript / synthesis frames ---- */
      .fast-embed-frame {
        width: 100%;
        display: block;
        border: 1px solid #e6edf4;
        border-radius: 14px;
        background: #ffffff;
        box-shadow: 0 1px 2px rgba(16, 40, 70, 0.04),
                    0 8px 20px rgba(16, 40, 70, 0.045);
      }

      .fast-tab-placeholder {
        max-width: 520px;
        margin: 40px auto;
        padding: 30px 28px;
        text-align: center;
        border: 1px solid #e8eef5;
        border-radius: 14px;
        background: #ffffff;
      }

      .fast-tab-placeholder-icon {
        width: 52px;
        height: 52px;
        margin: 0 auto 16px auto;
        border-radius: 14px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 20px;
        color: #4a7bb0;
        background: #eef4fb;
        border: 1px solid #e0eaf3;
      }

      .fast-tab-placeholder h4 {
        margin: 0 0 8px 0;
        font-size: 16px;
        font-weight: 700;
        color: #24405e;
      }

      .fast-tab-placeholder-text {
        margin: 0;
        font-size: 13px;
        line-height: 1.7;
        color: #8296ab;
      }

      /* ---- origin bar ---- */
      .fast-origin-wrap {
        max-width: 560px;
      }

      .fast-origin-bar {
        display: flex;
        height: 14px;
        border-radius: 999px;
        overflow: hidden;
        background: #eef2f7;
      }

      .fast-origin-seg { height: 100%; }
      .fast-origin-seg-endo { background: #21867f; }
      .fast-origin-seg-exo { background: #6fbfa4; }
      .fast-origin-seg-unknown { background: #dde5ec; }

      .fast-origin-legend {
        display: flex;
        flex-wrap: wrap;
        gap: 18px;
        margin-top: 11px;
      }

      .fast-origin-legend-item {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        font-size: 12px;
        font-weight: 600;
        color: #5c7186;
        font-variant-numeric: tabular-nums;
      }

      .fast-origin-dot {
        width: 10px;
        height: 10px;
        border-radius: 50%;
        flex: 0 0 auto;
      }

      .fast-origin-dot-endo { background: #21867f; }
      .fast-origin-dot-exo { background: #6fbfa4; }
      .fast-origin-dot-unknown { background: #dde5ec; }

      @media (max-width: 900px) {
        .fast-field {
          grid-template-columns: 1fr;
          gap: 6px;
        }

        .fast-function-hero h3 {
          font-size: 19px;
        }
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

      /* fluidPage wraps everything in .container-fluid; its gutters would
         stop the navbar and the analysis workbench from reaching the edges. */
      .container-fluid {
        padding-left: 0;
        padding-right: 0;
      }

      /* Content container */
      .content-wrapper {
        max-width: none;
        width: 100%;
        margin: 0 auto;
        padding: 0 28px;
      }

      /* Account page */
      #signin_page {
        background: #f7f8fa;
      }

      #signin_page .auth-page {
        min-height: calc(100vh - var(--fm-nav-h, 70px));
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 48px 24px;
      }

      #signin_page .auth-panel {
        width: 100%;
        max-width: 400px;
        border: 1px solid #e2e5ea;
        border-radius: 18px;
        background: #ffffff;
        box-shadow: 0 24px 48px -24px rgba(17, 24, 60, 0.25);
        padding: 36px 32px;
      }

      #signin_page .auth-title {
        margin: 0 0 22px 0;
        color: #1b1f24;
        font-size: 24px;
        font-weight: 800;
        line-height: 1.2;
        letter-spacing: 0;
      }

      #signin_page .auth-form {
        display: flex;
        flex-direction: column;
        gap: 16px;
      }

      #signin_page .auth-field {
        display: flex;
        flex-direction: column;
        gap: 7px;
        margin: 0;
        color: #55607a;
        font-size: 13px;
        font-weight: 700;
      }

      #signin_page .auth-required-label {
        display: inline-flex;
        align-items: baseline;
        gap: 4px;
      }

      #signin_page .auth-required-mark {
        color: #c0392b;
      }

      #signin_page .auth-field-hint {
        color: #7b8498;
        font-size: 11.5px;
        font-weight: 600;
        line-height: 1.4;
      }

      #signin_page .auth-label-row {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 16px;
        width: 100%;
      }

      #signin_page .auth-inline-link,
      #signin_page .auth-inline-link:visited {
        color: #0f7c96;
        font-size: 12.5px;
        font-weight: 700;
        text-decoration: none;
      }

      #signin_page .auth-inline-link:hover,
      #signin_page .auth-inline-link:focus {
        color: #0b647a;
        text-decoration: underline;
      }

      #signin_page .auth-input-wrap {
        position: relative;
        display: flex;
        align-items: center;
      }

      #signin_page .auth-input-wrap > .fa,
      #signin_page .auth-input-wrap > .fas,
      #signin_page .auth-input-wrap > .far {
        position: absolute;
        left: 13px;
        width: 18px;
        color: #9aa1c2;
        text-align: center;
        pointer-events: none;
      }

      #signin_page .auth-input-wrap .form-control {
        width: 100%;
        min-height: 44px;
        padding: 11px 14px 11px 40px;
        border: 1.5px solid #e2e5ea;
        border-radius: 10px;
        background: #fbfbfd;
        color: #1b1f24;
        font-size: 15px;
        box-shadow: none;
        transition: border-color 0.15s ease, background-color 0.15s ease;
      }

      #signin_page .auth-input-wrap .form-control.has-trailing-btn {
        padding-right: 42px;
      }

      #signin_page .auth-input-wrap .form-control:focus {
        background: #ffffff;
        border-color: #519d9e;
        box-shadow: none;
        outline: none;
      }

      #signin_page .auth-password-toggle {
        position: absolute;
        right: 6px;
        width: 30px;
        height: 30px;
        border: none;
        border-radius: 8px;
        background: transparent;
        color: #9aa1c2;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        padding: 0;
      }

      #signin_page .auth-password-toggle:hover,
      #signin_page .auth-password-toggle:focus {
        background: #eef7fa;
        color: #0f7c96;
      }

      #signin_page .btn-auth-primary {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
        width: 100%;
        min-height: 45px;
        margin-top: 4px;
        border: none;
        border-radius: 10px;
        background: #0f7c96;
        color: #ffffff;
        font-size: 15px;
        font-weight: 800;
        box-shadow: 0 10px 20px rgba(15, 124, 150, 0.22);
        transform: translateY(0);
        transition: background-color 0.15s ease, box-shadow 0.15s ease, transform 0.12s ease;
      }

      #signin_page .btn-auth-primary:hover,
      #signin_page .btn-auth-primary:focus {
        background: #0b647a;
        color: #ffffff;
        box-shadow: 0 12px 24px rgba(15, 124, 150, 0.28);
        transform: translateY(-1px);
      }

      #signin_page .btn-auth-primary:active,
      #signin_page .btn-auth-primary.is-auth-pressing {
        background: #09596d;
        box-shadow: 0 5px 12px rgba(15, 124, 150, 0.22);
        transform: translateY(1px);
      }

      #signin_page .btn-auth-primary:disabled,
      #signin_page .btn-auth-secondary:disabled,
      #signin_page .btn-auth-primary.disabled,
      #signin_page .btn-auth-secondary.disabled {
        cursor: not-allowed;
        opacity: 0.58;
        box-shadow: none;
        transform: none;
      }

      #signin_page .auth-switch {
        margin: 22px 0 0 0;
        text-align: center;
        color: #6b7280;
        font-size: 14px;
      }

      #signin_page .auth-switch a {
        color: #0f7c96;
        font-weight: 800;
        text-decoration: none;
      }

      #signin_page .auth-switch a:hover,
      #signin_page .auth-switch a:focus {
        text-decoration: underline;
      }

      #signin_page .auth-alert {
        display: flex;
        align-items: flex-start;
        gap: 8px;
        margin: 0 0 16px 0;
        padding: 0;
        border-radius: 0;
        border: none;
        background: transparent;
        font-size: 13.5px;
        line-height: 1.5;
      }

      #signin_page .auth-alert-error {
        color: #c0392b;
      }

      #signin_page .auth-alert-success {
        color: #1a7f37;
      }

      #signin_page .auth-alert-info {
        color: #0f7c96;
      }

      #signin_page .auth-alert .fa,
      #signin_page .auth-alert .fas,
      #signin_page .auth-alert .far {
        flex: 0 0 auto;
        width: 17px;
        margin-top: 2px;
      }

      #signin_page .auth-success {
        display: grid;
        gap: 16px;
        text-align: center;
      }

      #signin_page .auth-success-mark {
        width: 58px;
        height: 58px;
        margin: 0 auto;
        border-radius: 50%;
        display: grid;
        place-items: center;
        background: #edf8f5;
        color: #1a7f37;
        font-size: 24px;
      }

      #signin_page .auth-success-title {
        margin: 0;
        color: #1b1f24;
        font-size: 24px;
        font-weight: 800;
      }

      #signin_page .auth-success-text {
        margin: 0;
        color: #5f6f86;
        font-size: 15px;
        line-height: 1.55;
      }

      #signin_page .auth-success-email {
        color: #20364d;
        font-weight: 800;
        overflow-wrap: anywhere;
      }

      #signin_page .auth-success-actions {
        display: grid;
        gap: 10px;
      }

      #signin_page .auth-account {
        display: grid;
        gap: 16px;
        text-align: center;
      }

      #signin_page .auth-account-mark {
        width: 58px;
        height: 58px;
        margin: 0 auto;
        border-radius: 50%;
        display: grid;
        place-items: center;
        background: #e6f2f4;
        color: #2b7180;
        font-size: 24px;
      }

      #signin_page .auth-account-title {
        margin: 0;
        color: #1b1f24;
        font-size: 24px;
        font-weight: 800;
      }

      #signin_page .auth-account-meta {
        margin: 4px 0 0 0;
        color: #687b93;
        font-size: 14px;
      }

      #signin_page .auth-account-actions {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
      }

      #signin_page .btn-auth-secondary {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 100%;
        min-height: 44px;
        border: 1.5px solid #e2e5ea;
        border-radius: 10px;
        background: #ffffff;
        color: #0f7c96;
        font-weight: 800;
        box-shadow: 0 1px 3px rgba(31, 54, 82, 0.08);
        transform: translateY(0);
        transition: background-color 0.15s ease, border-color 0.15s ease, color 0.15s ease, box-shadow 0.15s ease, transform 0.12s ease;
      }

      #signin_page .btn-auth-secondary:hover,
      #signin_page .btn-auth-secondary:focus {
        background: #f1f9fb;
        border-color: #b6d5de;
        color: #0b647a;
        box-shadow: 0 8px 18px rgba(31, 54, 82, 0.12);
        transform: translateY(-1px);
      }

      #signin_page .btn-auth-secondary:active,
      #signin_page .btn-auth-secondary.is-auth-pressing {
        background: #e6f4f7;
        border-color: #9bc6d1;
        box-shadow: 0 2px 7px rgba(31, 54, 82, 0.10);
        transform: translateY(1px);
      }

      #signin_page .btn-auth-primary:disabled:hover,
      #signin_page .btn-auth-primary.disabled:hover,
      #signin_page .btn-auth-secondary:disabled:hover,
      #signin_page .btn-auth-secondary.disabled:hover {
        box-shadow: none;
        transform: none;
      }

      @media (max-width: 640px) {
        #signin_page .auth-page {
          align-items: flex-start;
          padding: 24px 14px;
        }

        #signin_page .auth-panel {
          padding: 24px 18px;
        }

        #signin_page .auth-account-actions {
          width: 100%;
        }

        #signin_page .auth-account-actions {
          grid-template-columns: 1fr;
        }
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
        margin-bottom: 18px;
      }

      #results_page .results-overview-card {
        background: white;
        border: 1px solid #dbe4f1;
        border-radius: 8px;
        padding: 14px 16px;
        box-shadow: 0 1px 2px rgba(34, 66, 105, 0.06);
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

      #results_page .results-download-btn {
        font-size: 17px;
        font-weight: 700;
        border-radius: 10px;
      }

      #results_page .results-empty-preview {
        margin: 0;
        font-size: 13px;
        color: #6b7684;
      }

      #results_page .results-network-wrap {
        margin-top: 14px;
        border: 1px solid #d6e3f2;
        border-radius: 10px;
        padding: 12px;
        background: #f7fbff;
      }

      #results_page .run-next-row {
        margin-top: 18px;
      }

      /* ---- run-parameter spec sheet ---- */
      #results_page #results_params_card .wb-scroll-area {
        gap: 18px;
        overflow-y: auto;
        padding: 22px;
        background: #f6f8fb;
      }

      #results_page .rp-wrap {
        display: flex;
        flex-direction: column;
        gap: 14px;
      }

      #results_page .rp-topbar {
        padding: 2px 2px 14px 2px;
        border-bottom: 1px solid #dfe7f0;
      }

      #results_page .rp-title {
        margin: 0;
        font-size: 20px;
        line-height: 1.2;
        font-weight: 750;
        color: #1c3450;
      }

      #results_page .rp-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(270px, 1fr));
        gap: 12px;
        align-items: stretch;
      }

      #results_page .rp-card {
        background: #fff;
        border: 1px solid #dfe7f0;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 1px 2px rgba(18, 38, 66, 0.05);
      }

      #results_page .rp-head {
        display: flex;
        align-items: center;
        gap: 11px;
        padding: 12px 14px;
        background: #fbfcfe;
        border-bottom: 1px solid #edf2f7;
      }

      #results_page .rp-num {
        flex: 0 0 25px;
        width: 25px;
        height: 25px;
        border-radius: 8px;
        background: #eaf1f9;
        border: 1px solid #d8e6f4;
        color: #2f6fb0;
        font-size: 11px;
        font-weight: 700;
        display: flex;
        align-items: center;
        justify-content: center;
      }

      #results_page .rp-name {
        font-size: 13.5px;
        font-weight: 700;
        color: #1c3450;
        letter-spacing: -0.005em;
      }

      #results_page .rp-status {
        margin-left: auto;
        flex-shrink: 0;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.1em;
        text-transform: uppercase;
        padding: 3px 10px;
        border-radius: 999px;
      }

      #results_page .rp-status.is-done {
        background: #e8f5ed;
        color: #2e8055;
        border: 1px solid #d1e7db;
      }

      #results_page .rp-status.is-skip {
        background: #eef1f5;
        color: #7b8b9f;
        border: 1px solid #e2e8f0;
      }

      #results_page .rp-status.is-pending {
        background: #fdf3e7;
        color: #b56c2b;
        border: 1px solid #f0dcc4;
      }

      #results_page .rp-row {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 16px;
        padding: 9px 14px;
        border-bottom: 1px solid #f2f6fa;
      }

      #results_page .rp-row:last-child {
        border-bottom: none;
      }

      #results_page .rp-label {
        font-size: 12.5px;
        color: #8296ab;
        flex: 0 1 auto;
      }

      #results_page .rp-value {
        font-size: 13px;
        font-weight: 600;
        color: #1c3450;
        text-align: right;
        word-break: break-word;
        font-variant-numeric: tabular-nums;
      }

      #results_page .rp-empty {
        padding: 13px 16px;
        font-size: 12.5px;
        color: #93a5b8;
      }

      #results_page .rp-none {
        font-size: 13px;
        color: #8296ab;
        margin: 0;
        padding: 16px;
        background: #ffffff;
        border: 1px solid #dfe7f0;
        border-radius: 8px;
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
      /* vis.js tooltip: a long InChI on one line ran off the canvas */
      div.vis-tooltip {
        max-width: 420px;
        white-space: normal !important;
        overflow-wrap: anywhere;
        word-break: break-word;
        padding: 10px 12px;
        border: 1px solid #d7e1ec;
        border-radius: 10px;
        background: #ffffff;
        box-shadow: 0 12px 26px rgba(28, 49, 74, 0.18);
        font-family: inherit;
        font-size: 12px;
        line-height: 1.5;
        color: #415366;
      }

      .pcsf-tip-kind {
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: #8fa3b8;
        margin-bottom: 3px;
      }

      .pcsf-tip-name {
        font-size: 13px;
        font-weight: 700;
        color: #1c3450;
        margin-bottom: 5px;
      }

      .pcsf-tip-id {
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 11px;
        color: #6d8299;
        overflow-wrap: anywhere;
      }

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
        var GAP = 12;
        var EDGE = 8;

        // Same placement rule as the quick-start dialog: sit beside the
        // anchor, vertically centred on it, flipping side or dropping
        // below when there is no room. The card is position:fixed, so a
        // scrolling sidebar can never clip it.
        function placeCard(wrap) {
          var card = wrap.querySelector('.inline-info-card');
          if (!card) return;

          card.style.position = 'fixed';
          card.style.margin = '0';
          card.style.transform = 'none';
          card.style.visibility = 'hidden';
          card.style.display = 'block';
          card.style.left = '0px';
          card.style.top = '0px';

          var tr = wrap.getBoundingClientRect();
          var vw = window.innerWidth || document.documentElement.clientWidth;
          var vh = window.innerHeight || document.documentElement.clientHeight;
          var dw = card.offsetWidth || 320;
          var dh = card.offsetHeight || 160;

          var left = tr.right + GAP;
          var top = tr.top + (tr.height - dh) / 2;

          if (left + dw + EDGE > vw) {
            left = tr.left - dw - GAP;
          }
          if (left < EDGE) {
            left = Math.max(EDGE, vw - dw - EDGE);
          }
          if (top + dh + EDGE > vh) {
            top = vh - dh - EDGE;
          }
          if (top < EDGE) {
            top = EDGE;
          }

          // Still overlapping the anchor? Drop below it, else above.
          if (
            left < tr.right &&
            left + dw > tr.left &&
            top < tr.bottom &&
            top + dh > tr.top
          ) {
            top = tr.bottom + GAP;
            if (top + dh + EDGE > vh) {
              top = Math.max(EDGE, tr.top - dh - GAP);
            }
          }

          card.style.left = Math.round(left) + 'px';
          card.style.top = Math.round(top) + 'px';
          card.style.removeProperty('display');
          card.style.removeProperty('visibility');
        }

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
            event.preventDefault();
            event.stopPropagation();
            var shouldPin = !trigger.classList.contains('is-pinned');
            clearPinnedInfo(trigger);
            if (shouldPin) {
              trigger.classList.add('is-pinned');
              placeCard(trigger);
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
    ")),

    tags$script(HTML("
      window.closeAnalysisMenu = function() {
        var dd = document.getElementById('analysis_dropdown');
        if (dd) { dd.classList.remove('open'); }
      };

      window.toggleAnalysisMenu = function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        var dd = document.getElementById('analysis_dropdown');
        if (dd) { dd.classList.toggle('open'); }
      };

      document.addEventListener('click', function(e) {
        var dd = document.getElementById('analysis_dropdown');
        if (dd && !dd.contains(e.target)) { dd.classList.remove('open'); }
      });

      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') { window.closeAnalysisMenu(); }
      });
    ")),

    tags$script(HTML("
      (function() {
        function setNavHeight() {
          var bar = document.querySelector('.top-navbar');
          if (!bar) return;
          var h = Math.ceil(bar.getBoundingClientRect().height);
          if (h > 0) {
            document.documentElement.style.setProperty('--fm-nav-h', h + 'px');
          }
        }
        document.addEventListener('DOMContentLoaded', function() {
          setNavHeight();
          setTimeout(setNavHeight, 250);
        });
        window.addEventListener('resize', setNavHeight);
        window.setFeaturemapNavHeight = setNavHeight;
      })();
    "))
    
    
    
  ),
  
  # Top Navigation Bar
  tags$div(
    class = "top-navbar",
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
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
        tags$li(
          class = "nav-item",
          tags$a(
            id = "nav_about",
            class = "nav-link",
            href = "#",
            onclick = paste0(
              "Shiny.setInputValue('footer_nav', 'home', {priority: 'event'}); ",
              "setTimeout(function(){ ",
              "document.querySelector('#home_page .workflow-section')",
              "?.scrollIntoView({behavior: 'smooth', block: 'start'}); ",
              "}, 120); return false;"
            ),
            "About"
          )
        ),
        tags$li(
          class = "nav-item nav-dropdown",
          id = "analysis_dropdown",
          tags$a(
            id = "nav_analysis",
            class = "nav-link nav-dropdown-toggle",
            href = "#",
            onclick = "window.toggleAnalysisMenu(event); return false;",
            "Analysis",
            icon("chevron-down", class = "nav-caret")
          ),
          tags$ul(
            class = "nav-dropdown-menu",
            tags$li(
              actionLink(
                "nav_upload",
                tagList(
                  tags$span(class = "nav-step-num", "1"),
                  tags$span(class = "nav-step-label", "Data Upload")
                ),
                class = "nav-dropdown-item"
              )
            ),
            tags$li(
              actionLink(
                "nav_annotation",
                tagList(
                  tags$span(class = "nav-step-num", "2"),
                  tags$span(class = "nav-step-label", "Feature Annotation")
                ),
                class = "nav-dropdown-item"
              )
            ),
            tags$li(
              actionLink(
                "nav_network",
                tagList(
                  tags$span(class = "nav-step-num", "3"),
                  tags$span(class = "nav-step-label", "PCSF Subnetwork")
                ),
                class = "nav-dropdown-item"
              )
            ),
            tags$li(
              actionLink(
                "nav_modules",
                tagList(
                  tags$span(class = "nav-step-num", "4"),
                  tags$span(class = "nav-step-label", "Module Identification")
                ),
                class = "nav-dropdown-item"
              )
            ),
            tags$li(
              actionLink(
                "nav_function",
                tagList(
                  tags$span(class = "nav-step-num", "5"),
                  tags$span(class = "nav-step-label", "Function Interpretation")
                ),
                class = "nav-dropdown-item"
              )
            ),
            tags$li(
              actionLink(
                "nav_results",
                tagList(
                  tags$span(class = "nav-step-num", "6"),
                  tags$span(class = "nav-step-label", "Results Download")
                ),
                class = "nav-dropdown-item"
              )
            )
          )
        ),
        tags$li(class = "nav-item", actionLink("nav_authors", "Authors", class = "nav-link")),
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link",
            href = "https://www.shen-lab.org/contact/",
            target = "_blank",
            rel = "noopener noreferrer",
            "Contacts"
          )
        ),
        tags$li(class = "nav-item nav-auth-item", uiOutput("auth_nav", inline = TRUE))
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
          tags$h1(class = "hero-title", "FeatureMAP Online"),
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
              class = "btn btn-hero btn-hero-secondary",
              href = "https://tidymass.github.io/featuremap_tutorial/",
              target = "_blank",
              rel = "noopener noreferrer",
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
                tags$div(class = "feature-title", "Comprehensive Network")
              )
            )
          )
        ),

        # Workflow Section
        tags$div(
          class = "workflow-section home-section",
          tags$div(
            class = "home-section-head",
            tags$h2(class = "section-title", "About FeatureMAP")
          ),
          tags$div(
            class = "workflow-panel",
            tags$div(
              class = "workflow-copy",
              tags$div(
                class = "workflow-copy-grid",
                tags$div(
                  tags$p(class = "workflow-kicker", "Feature-centric metabolic interpretation"),
                  tags$h3(
                    class = "workflow-lede-title",
                    "From LC-MS features to dysregulated metabolic modules"
                  )
                ),
                tags$div(
                  tags$p(
                    class = "workflow-lede",
                    "FeatureMAP is a feature-centric, metabolic network-guided framework for interpreting untargeted LC-MS metabolomics data without requiring complete metabolite identification or an arbitrary significant-feature cutoff. It takes a conventional feature table with m/z, retention time, ionization mode, and phenotype association as input, then converts partially annotated feature signals into network-refined annotations and dysregulated metabolic modules."
                  ),
                  tags$p(
                    class = "workflow-lede",
                    "The workflow integrates multi-evidence feature-to-metabolite annotation, cutoff-free feature weighting, PCSF-based network refinement, robustness-weighted subnetwork construction, community detection, module scoring, and structured LLM-assisted interpretation. Final outputs include refined feature-metabolite assignments, statistically supported modules, functional summaries, confidence rationales, and prioritized metabolites for downstream validation."
                  ),
                  tags$p(
                    class = "workflow-credit",
                    tags$strong("Citation"),
                    tags$br(),
                    tags$em(
                      "FeatureMAP enables network-guided discovery and interpretation of dysregulated metabolic modules from untargeted metabolomics"
                    ),
                    tags$br(),
                    HTML(
                      "Yuting Wang<sup>+</sup>, Feifan Zhang<sup>+</sup>, Guoping Peng, Zeng Li, Yijiang Liu, Jiramet Kinchagawat, Xicheng Yang, Tao Huan, Chang Liu, Caroline Johnson, Qi Su, Chuchu Wang, and Xiaotao Shen<sup>*</sup>"
                    ),
                    tags$br(),
                    tags$span(
                      style = "font-style: italic; color: #7a8aa0; font-size: 0.92em;",
                      "(Manuscript in preparation; + equal contribution; * corresponding author)"
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "workflow-figure",
              tags$img(
                src = "workflow_overview.png",
                class = "workflow-image",
                alt = "FeatureMAP workflow overview"
              )
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
        class = "workbench",

        # ================= Header rail =================
        tags$div(
          class = "wb-header",
          tags$div(
            class = "wb-header-left",
            tags$span(class = "wb-eyebrow", icon("upload"), "Step 1 of 6"),
            tags$span(class = "wb-header-divider"),
            tags$h1(class = "wb-title", "Data Upload")
          ),
          tags$ol(
            class = "wb-stepper",
            tags$li(
              class = "wb-step is-current",
              title = "Data Upload",
              tags$span(class = "wb-step-dot", "1"),
              tags$span(class = "wb-step-name", "Upload")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Feature Annotation",
              tags$span(class = "wb-step-dot", "2"),
              tags$span(class = "wb-step-name", "Annotate")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "PCSF Subnetwork",
              tags$span(class = "wb-step-dot", "3"),
              tags$span(class = "wb-step-name", "Network")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Module Identification",
              tags$span(class = "wb-step-dot", "4"),
              tags$span(class = "wb-step-name", "Modules")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Function Interpretation",
              tags$span(class = "wb-step-dot", "5"),
              tags$span(class = "wb-step-name", "Function")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Results Download",
              tags$span(class = "wb-step-dot", "6"),
              tags$span(class = "wb-step-name", "Results")
            )
          )
        ),

        # ================= Sidebar + work area =================
        tags$div(
          class = "wb-body",

          # ---------------- Sidebar: inputs ----------------
          tags$div(
            id = "upload_param_card",
            class = "wb-side",
            tags$div(
              class = "wb-side-scroll",

              tags$div(
                class = "wb-section",
                tags$div(class = "wb-section-title", icon("table-list"), "Data Tables"),
                tags$div(
                  class = "wb-field-label",
                  "Feature Table (.rda, .csv)",
                  build_info_icon(
                    paste0(
                      "<b>Required columns</b><br>",
                      "<code>variable_id</code>, <code>mz</code>, <code>rt</code>, ",
                      "<code>polarity</code>, <code>mean_intensity</code>, ",
                      "<code>condition</code>, <code>pvalue</code>.<br><br>",
                      "Maximum upload size: ", featuremap_max_upload_label(), " per file."
                    )
                  )
                ),
                fileInput(
                  "feature_file",
                  NULL,
                  accept = c(".rda", ".csv", ".Rdata")
                ),

                tags$div(
                  class = "wb-subfield",
                  tags$div(
                    class = "wb-field-label",
                    "MS2 Annotation (.rda, .csv)",
                    build_info_icon(
                      paste0(
                        "Optional.<br><br>",
                        "<b>Required columns</b><br>",
                        "<code>variable_id</code>, and at least one of ",
                        "<code>HMDB.ID</code> or <code>KEGG.ID</code>.<br><br>",
                        "Maximum upload size: ", featuremap_max_upload_label(), " per file."
                      )
                    )
                  ),
                  fileInput(
                    "ms2_file",
                    NULL,
                    accept = c(".rda", ".csv", ".Rdata")
                  )
                ),

                tags$div(
                  class = "wb-inline-actions",
                  actionButton(
                    "use_demo_both",
                    "Use Demo Data",
                    icon = icon("flask"),
                    class = "btn btn-wb-ghost"
                  ),
                  downloadButton(
                    "download_demo_data",
                    "Download Demo",
                    class = "btn btn-wb-ghost"
                  )
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("database"),
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
                  NULL,
                  choices = c(
                    "Default reference (recommended)" = "default",
                    "Upload reference datasets" = "upload"
                  ),
                  selected = "default"
                ),
                conditionalPanel(
                  condition = "input.ref_source == 'upload'",
                  tags$div(
                    class = "wb-subfield",
                    tags$div(
                      class = "wb-field-label",
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
                conditionalPanel(
                  condition = "input.ref_source == 'upload'",
                  tags$div(
                    class = "wb-subfield",
                    tags$div(
                      class = "wb-field-label",
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
              )
            ),

            # ---------------- Sticky actions ----------------
            tags$div(
              class = "wb-actions",
              actionButton(
                "load_upload_data",
                "Load",
                icon = icon("upload"),
                class = "btn btn-wb-primary"
              ),
              actionButton(
                "next_to_annotation",
                "Next Step",
                icon = icon("arrow-right-from-bracket"),
                class = "btn btn-wb-next",
                disabled = "disabled"
              )
            )
          ),

          # ---------------- Work area: preview + status ----------------
          tags$div(
            id = "upload_result_card",
            class = "wb-main",
            tags$div(
              class = "wb-main-inner wb-preview-shell",
              tabsetPanel(
                id = "upload_preview_tabs",
                tabPanel(
                  "Feature Table",
                  tags$div(
                    class = "wb-preview-table",
                    DTOutput("feature_preview")
                  )
                ),
                tabPanel(
                  "MS2 Table",
                  tags$div(
                    class = "wb-preview-table",
                    DTOutput("ms2_preview")
                  )
                )
              )
            ),
            uiOutput("upload_empty_state")
          )
        )
      )
    ),
    
    # Annotation Page  
    tags$div(
      id = "annotation_page",
      style = "display: none;",
      tags$div(
        class = "workbench",

        # ================= Header rail =================
        tags$div(
          class = "wb-header",
          tags$div(
            class = "wb-header-left",
            tags$span(class = "wb-eyebrow", icon("magnifying-glass"), "Step 2 of 6"),
            tags$span(class = "wb-header-divider"),
            tags$h1(class = "wb-title", "Feature Annotation")
          ),
          tags$ol(
            class = "wb-stepper",
            tags$li(
              class = "wb-step is-done",
              title = "Data Upload",
              tags$span(class = "wb-step-dot", "1"),
              tags$span(class = "wb-step-name", "Upload")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-current",
              title = "Feature Annotation",
              tags$span(class = "wb-step-dot", "2"),
              tags$span(class = "wb-step-name", "Annotate")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "PCSF Subnetwork",
              tags$span(class = "wb-step-dot", "3"),
              tags$span(class = "wb-step-name", "Network")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Module Identification",
              tags$span(class = "wb-step-dot", "4"),
              tags$span(class = "wb-step-name", "Modules")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Function Interpretation",
              tags$span(class = "wb-step-dot", "5"),
              tags$span(class = "wb-step-name", "Function")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Results Download",
              tags$span(class = "wb-step-dot", "6"),
              tags$span(class = "wb-step-name", "Results")
            )
          )
        ),

        # ================= Sidebar + work area =================
        tags$div(
          class = "wb-body",

          # ---------------- Sidebar: parameters ----------------
          tags$div(
            id = "annotation_param_card",
            class = "wb-side",
            tags$div(
              class = "wb-side-scroll",

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("ruler-horizontal"),
                  "Matching Tolerance",
                  build_info_icon(
                    paste0(
                      "Windows used to match each feature against the MS1 ",
                      "annotation database.<br><br>",
                      "Tighter values give fewer but more confident matches."
                    )
                  )
                ),
                numericInput(
                  "ms1_ppm", "MS1 match (ppm)",
                  value = 15, min = 0, step = 1
                ),
                numericInput(
                  "rt_tol", "RT tolerance (second)",
                  value = 5, min = 0, step = 1
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("sliders"),
                  "Advanced Options",
                  build_info_icon(
                    paste0(
                      "<b>Column type</b> selects the retention-time model.<br><br>",
                      "<b>Isotope number</b> is how many isotope peaks are ",
                      "annotated per feature.<br><br>",
                      "<b>Threads</b> controls parallel workers."
                    )
                  )
                ),
                selectInput(
                  "column_type", "Column type",
                  choices = c("Reverse Phase" = "rp", "HILIC" = "hilic")
                ),
                numericInput("isotope_num", "Isotope number", value = 3, min = 1, max = 5),
                numericInput("workers", "Threads", value = 4, min = 1, max = 16)
              )
            ),

            # ---------------- Sticky actions ----------------
            tags$div(
              class = "wb-actions",
              actionButton(
                "run_annotation",
                "Run",
                icon = icon("play"),
                class = "btn btn-wb-primary"
              ),
              actionButton(
                "next_to_network",
                "Next Step",
                icon = icon("arrow-right-from-bracket"),
                class = "btn btn-wb-next",
                disabled = "disabled"
              )
            )
          ),

          # ---------------- Work area ----------------
          tags$div(
            id = "annotation_result_card",
            class = "wb-main",
            tags$div(
              class = "wb-main-inner wb-preview-shell",
              tabsetPanel(
                id = "annotation_tabs",
                type = "tabs",
                tabPanel(
                  title = "Summary",
                  value = "summary",
                  tags$div(
                    class = "wb-plot-stack",
                    uiOutput("annotation_summary_ui")
                  )
                ),
                tabPanel(
                  title = "Annotation Table",
                  value = "table1",
                  tags$div(
                    class = "wb-preview-table",
                    DTOutput("annotated_table")
                  )
                )
              )
            ),
            uiOutput("annotation_empty_state")
          )
        )
      )
    ),

    # ================= Step 3: Subnetwork Extraction =================
    tags$div(
      id = "network_page",
      style = "display: none;",
      tags$div(
        class = "workbench",

        tags$div(
          class = "wb-header",
          tags$div(
            class = "wb-header-left",
            tags$span(class = "wb-eyebrow", icon("network-wired"), "Step 3 of 6"),
            tags$span(class = "wb-header-divider"),
            tags$h1(class = "wb-title", "Subnetwork Extraction")
          ),
          tags$ol(
            class = "wb-stepper",
            tags$li(
              class = "wb-step is-done",
              title = "Data Upload",
              tags$span(class = "wb-step-dot", "1"),
              tags$span(class = "wb-step-name", "Upload")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Feature Annotation",
              tags$span(class = "wb-step-dot", "2"),
              tags$span(class = "wb-step-name", "Annotate")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-current",
              title = "PCSF Subnetwork",
              tags$span(class = "wb-step-dot", "3"),
              tags$span(class = "wb-step-name", "Network")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Module Identification",
              tags$span(class = "wb-step-dot", "4"),
              tags$span(class = "wb-step-name", "Modules")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Function Interpretation",
              tags$span(class = "wb-step-dot", "5"),
              tags$span(class = "wb-step-name", "Function")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Results Download",
              tags$span(class = "wb-step-dot", "6"),
              tags$span(class = "wb-step-name", "Results")
            )
          )
        ),

        tags$div(
          class = "wb-body",

          # ---------------- Sidebar ----------------
          tags$div(
            id = "network_param_card",
            class = "wb-side",
            tags$div(
              class = "wb-side-scroll",

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("filter"),
                  "Feature Selection",
                  build_info_icon(
                    paste0(
                      "Which annotated features are handed to PCSF as terminals ",
                      "(the nodes the algorithm is rewarded for keeping).<br><br>",
                      "<b>Significant only</b> uses features below the p-value cutoff; ",
                      "<b>All features</b> lets every feature carry a prize."
                    )
                  )
                ),
                selectInput(
                  "pcsf_feature",
                  "Features used as terminals",
                  choices = c("Significant only" = "sig", "All features" = "all"),
                  selected = "sig"
                ),
                conditionalPanel(
                  condition = "input.pcsf_feature == 'sig'",
                  numericInput(
                    "pcsf_pcutoff",
                    "Significance cutoff (p-value)",
                    value = 0.05, min = 0, max = 1, step = 0.01
                  )
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("sliders"),
                  "PCSF Parameters",
                  build_info_icon(
                    paste0(
                      "<b>b</b> scales the node prizes: higher keeps more terminals.<br><br>",
                      "<b>w</b> penalises extra Steiner trees: higher gives fewer, ",
                      "larger connected components.<br><br>",
                      "<b>Grid tuning</b> sweeps a range of b and w and keeps the ",
                      "most robust combination."
                    )
                  )
                ),
                checkboxInput("pcsf_grid_tune", "Enable grid tuning", value = FALSE),

                conditionalPanel(
                  condition = "input.pcsf_grid_tune",
                  tags$div(
                    class = "wb-subfield",
                    tags$div(class = "wb-field-label", "Grid tuning range"),
                    fluidRow(
                      column(4, numericInput("pcsf_b_min", "b min", value = 20, min = 0, step = 1)),
                      column(4, numericInput("pcsf_b_max", "b max", value = 40, min = 0, step = 1)),
                      column(4, numericInput("pcsf_b_step", "b step", value = 5, min = 1, step = 1))
                    ),
                    fluidRow(
                      column(4, numericInput("pcsf_w_min", "w min", value = 20, min = 0, step = 1)),
                      column(4, numericInput("pcsf_w_max", "w max", value = 40, min = 0, step = 1)),
                      column(4, numericInput("pcsf_w_step", "w step", value = 5, min = 1, step = 1))
                    )
                  )
                ),

                conditionalPanel(
                  condition = "!input.pcsf_grid_tune",
                  numericInput("pcsf_b", "Node prize weight (b)", min = 20, max = 60, value = 20, step = 1),
                  numericInput("pcsf_w", "Steiner penalty (w)", min = 20, max = 60, value = 25, step = 5)
                ),

                numericInput("pcsf_times", "Iterations (final)", value = 100, min = 1, step = 1),

                conditionalPanel(
                  condition = "input.pcsf_grid_tune",
                  numericInput("pcsf_times_tune", "Iterations (grid tuning)", value = 10, min = 1, step = 1)
                ),

                numericInput("pcsf_workers", "Threads", value = 4, min = 1, max = 16),
                checkboxInput("pcsf_show_network", "Visualize PCSF subnetwork", value = TRUE)
              )
            ),

            tags$div(
              class = "wb-actions",
              actionButton(
                "run_pcsf", "Run PCSF",
                icon = icon("play"), class = "btn btn-wb-primary"
              ),
              actionButton(
                "next_to_modules", "Next Step",
                icon = icon("arrow-right-from-bracket"),
                class = "btn btn-wb-next", disabled = "disabled"
              )
            )
          ),

          # ---------------- Work area ----------------
          tags$div(
            id = "network_result_card",
            class = "wb-main",
            tags$div(
              class = "wb-main-inner wb-preview-shell",
              tabsetPanel(
                id = "pcsf_tabs",
                type = "tabs",
                tabPanel(
                  title = "Subnetwork",
                  tags$div(
                    class = "pcsf-network-shell",
                    tags$div(
                      class = "pcsf-network-topbar",
                      uiOutput("pcsf_node_selectors"),
                      uiOutput("pcsf_network_legend")
                    ),
                    uiOutput("pcsf_network_container")
                  )
                ),
                tabPanel(
                  title = "Annotation Table 2",
                  tags$div(
                    class = "wb-preview-table",
                    DTOutput("annotation_table2")
                  )
                )
              )
            ),
            uiOutput("network_empty_state")
          )
        )
      )
    ),

    # ================= Step 4: Module Identification =================
    tags$div(
      id = "modules_page",
      style = "display: none;",
      tags$div(
        class = "workbench",

        tags$div(
          class = "wb-header",
          tags$div(
            class = "wb-header-left",
            tags$span(class = "wb-eyebrow", icon("layer-group"), "Step 4 of 6"),
            tags$span(class = "wb-header-divider"),
            tags$h1(class = "wb-title", "Module Identification")
          ),
          tags$ol(
            class = "wb-stepper",
            tags$li(
              class = "wb-step is-done",
              title = "Data Upload",
              tags$span(class = "wb-step-dot", "1"),
              tags$span(class = "wb-step-name", "Upload")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Feature Annotation",
              tags$span(class = "wb-step-dot", "2"),
              tags$span(class = "wb-step-name", "Annotate")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "PCSF Subnetwork",
              tags$span(class = "wb-step-dot", "3"),
              tags$span(class = "wb-step-name", "Network")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-current",
              title = "Module Identification",
              tags$span(class = "wb-step-dot", "4"),
              tags$span(class = "wb-step-name", "Modules")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Function Interpretation",
              tags$span(class = "wb-step-dot", "5"),
              tags$span(class = "wb-step-name", "Function")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Results Download",
              tags$span(class = "wb-step-dot", "6"),
              tags$span(class = "wb-step-name", "Results")
            )
          )
        ),

        tags$div(
          class = "wb-body",

          # ---------------- Sidebar ----------------
          tags$div(
            id = "modules_param_card",
            class = "wb-side",
            tags$div(
              class = "wb-side-scroll",

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("diagram-project"),
                  "Algorithm",
                  build_info_icon(
                    paste0(
                      "Community-detection method run on the PCSF subnetwork.<br><br>",
                      "<b>Louvain</b> is fast and resolution-tunable; ",
                      "<b>Walktrap</b> uses random walks; ",
                      "<b>Fast greedy</b> and <b>Leading eigen</b> optimise modularity directly."
                    )
                  )
                ),
                selectInput(
                  "module_method",
                  "Community detection method",
                  choices = c(
                    "Louvain"       = "louvain",
                    "Walktrap"      = "walktrap",
                    "Fast greedy"   = "fast_greedy",
                    "Leading eigen" = "leading_eigen"
                  ),
                  selected = "louvain"
                ),
                conditionalPanel(
                  condition = "input.module_method == 'louvain'",
                  numericInput("louvain_res", "Resolution", value = 1, min = 0.1, max = 10)
                ),
                conditionalPanel(
                  condition = "input.module_method == 'walktrap'",
                  numericInput("walktrap_steps", "Number of steps", value = 4, min = 1, max = 10)
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("filter"),
                  "Filtering",
                  build_info_icon(
                    paste0(
                      "Nodes appearing in fewer than this fraction of PCSF iterations ",
                      "are dropped before community detection, which removes unstable ",
                      "parts of the subnetwork."
                    )
                  )
                ),
                sliderInput(
                  "node_freq", "Node frequency threshold",
                  min = 0, max = 1, value = 0.05, step = 0.01
                ),
                numericInput("min_module_size", "Minimum module size", value = 3, min = 2)
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("gauge-high"),
                  "Computation",
                  build_info_icon(
                    paste0(
                      "Module activity is tested against a null built by permutation. ",
                      "More permutations give a finer p-value at the cost of runtime; ",
                      "the seed makes a run reproducible."
                    )
                  )
                ),
                numericInput("n_perm", "Permutations", value = 100, min = 10, max = 1000),
                numericInput("module_seed", "Random seed", value = 123),
                numericInput("module_workers", "Threads", value = 4, min = 1, max = 16)
              )
            ),

            tags$div(
              class = "wb-actions",
              actionButton(
                "run_modules", "Run",
                icon = icon("play"), class = "btn btn-wb-primary"
              ),
              actionButton(
                "next_to_function", "Next Step",
                icon = icon("arrow-right-from-bracket"),
                class = "btn btn-wb-next", disabled = "disabled"
              )
            )
          ),

          # ---------------- Work area ----------------
          tags$div(
            id = "modules_result_card",
            class = "wb-main",
            tags$div(
              class = "wb-main-inner wb-preview-shell",
              tabsetPanel(
                id = "module_tabs",
                type = "tabs",

                tabPanel(
                  title = "Visualization",
                  tags$div(
                    class = "module-visual-shell",
                    tags$div(
                      class = "module-visual-filter-row",
                      tags$div(
                        id = "module_selector_ui",
                        class = "pcsf-selector-item",
                        tags$div(
                          class = "pcsf-selector-input",
                          tags$div(
                            class = "pcsf-selector-field",
                            selectizeInput(
                              "selected_module",
                              label = NULL,
                              choices = character(0),
                              selected = NULL,
                              multiple = FALSE,
                              width = "100%",
                              options = list(placeholder = "Module")
                            )
                          ),
                          build_info_icon("Select which detected module to display in the network below.")
                        )
                      ),
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
                              options = list(placeholder = "Metabolite")
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

                tabPanel(
                  title = "Module Score",
                  tags$div(
                    class = "wb-preview-table",
                    DTOutput("module_score")
                  )
                ),

                tabPanel(
                  title = "Module Table",
                  tags$div(
                    class = "wb-preview-table",
                    DTOutput("module_table")
                  )
                )
              )
            ),
            uiOutput("modules_empty_state")
          )
        )
      )
    ),

    # ================= Step 5: Function Interpretation =================
    tags$div(
      id = "function_page",
      style = "display: none;",
      tags$div(
        class = "workbench",

        tags$div(
          class = "wb-header",
          tags$div(
            class = "wb-header-left",
            tags$span(class = "wb-eyebrow", icon("robot"), "Step 5 of 6"),
            tags$span(class = "wb-header-divider"),
            tags$h1(class = "wb-title", "Function Interpretation")
          ),
          tags$ol(
            class = "wb-stepper",
            tags$li(
              class = "wb-step is-done",
              title = "Data Upload",
              tags$span(class = "wb-step-dot", "1"),
              tags$span(class = "wb-step-name", "Upload")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Feature Annotation",
              tags$span(class = "wb-step-dot", "2"),
              tags$span(class = "wb-step-name", "Annotate")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "PCSF Subnetwork",
              tags$span(class = "wb-step-dot", "3"),
              tags$span(class = "wb-step-name", "Network")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Module Identification",
              tags$span(class = "wb-step-dot", "4"),
              tags$span(class = "wb-step-name", "Modules")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-current",
              title = "Function Interpretation",
              tags$span(class = "wb-step-dot", "5"),
              tags$span(class = "wb-step-name", "Function")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step",
              title = "Results Download",
              tags$span(class = "wb-step-dot", "6"),
              tags$span(class = "wb-step-name", "Results")
            )
          )
        ),

        tags$div(
          class = "wb-body",

          # ---------------- Sidebar ----------------
          tags$div(
            id = "function_param_card",
            class = "wb-side",
            tags$div(
              class = "wb-side-scroll",

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("filter"),
                  "Module Selection",
                  build_info_icon("Only modules below this significance cutoff are sent to the model.")
                ),
                selectInput(
                  "func_p_type", "Significance metric",
                  choices = c("p-value" = "p", "FDR" = "fdr"), selected = "fdr"
                ),
                numericInput(
                  "func_p_cutoff", "Significance cutoff",
                  value = 0.05, min = 0, max = 1, step = 0.01
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("wand-magic-sparkles"),
                  "Annotation Mode",
                  build_info_icon(
                    paste0(
                      "<b>Fast</b>: single-hypothesis pipeline with pathway coverage, ",
                      "origin profiling, and directed confidence scoring.<br><br>",
                      "<b>Advanced</b>: multi-agent debate pipeline &mdash; a Proposer drafts 1-3 ",
                      "candidate hypotheses, a Skeptic challenges them (up to 2 rounds), and an ",
                      "Arbiter delivers final accepted/rejected verdicts with confidence scores."
                    )
                  )
                ),
                radioButtons(
                  "func_mode", NULL,
                  choices = c("Fast" = "fast", "Advanced" = "advanced"),
                  selected = "fast"
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("flask"),
                  "Study Context",
                  build_info_icon("Optional, but a species / biospecimen / phenotype makes the interpretation far more specific.")
                ),
                textInput("func_species", "Species", placeholder = "e.g. human"),
                textInput("func_biospecimen", "Biospecimen", placeholder = "e.g. plasma, serum, liver"),
                textInput("func_phenotype", "Phenotype", placeholder = "e.g. diabetes, cancer")
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("book"),
                  "Literature",
                  build_info_icon("Retrieves and embeds recent PubMed abstracts for each metabolite, then feeds the distilled evidence to the model.")
                ),
                checkboxInput("func_use_pubmed", "Use PubMed literature", value = FALSE),
                conditionalPanel(
                  condition = "input.func_use_pubmed",
                  numericInput("func_pubmed_years", "Years to search", value = 5, min = 1, max = 20),
                  passwordInput("func_ncbi_key", "NCBI API key")
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(
                  class = "wb-section-title",
                  icon("microchip"),
                  "Large Language Model",
                  build_info_icon(
                    paste0(
                      "Use the exact chat/generation-model ID supported by the selected provider.<br><br>",
                      "Model lists:<br>",
                      "<a href='https://platform.openai.com/docs/models' target='_blank'>OpenAI</a><br>",
                      "<a href='https://ai.google.dev/gemini-api/docs/models' target='_blank'>Gemini</a><br>",
                      "<a href='https://siliconflow.cn/models' target='_blank'>SiliconFlow</a>"
                    )
                  )
                ),
                selectInput(
                  "func_provider", "Provider",
                  choices = c(
                    "OpenAI" = "openai",
                    "Google Gemini" = "gemini",
                    "SiliconFlow" = "siliconflow",
                    "Local" = "local"
                  )
                ),
                textInput("func_llm_model", "Model ID", placeholder = "e.g. gpt-5.4"),
                conditionalPanel(
                  condition = "input.func_provider == 'local'",
                  textInput("func_base_url", "Local base URL", placeholder = "e.g. http://localhost:8000")
                ),
                passwordInput("func_llm_key", "API key (optional for Local)")
              )
            ),

            tags$div(
              class = "wb-actions",
              actionButton(
                "run_function_annotation", "Run",
                icon = icon("play"), class = "btn btn-wb-primary"
              ),
              actionButton(
                "next_to_results", "Results",
                icon = icon("download"), class = "btn btn-wb-next"
              )
            )
          ),

          # ---------------- Work area ----------------
          tags$div(
            id = "function_result_card",
            class = "wb-main",
            tags$div(
              class = "wb-main-inner wb-scroll-area",
              uiOutput("function_main_ui")
            ),
            uiOutput("function_empty_state")
          )
        )
      )
    ),

    # ================= Step 6: Results Download =================
    tags$div(
      id = "results_page",
      style = "display: none;",
      tags$div(
        class = "workbench",

        tags$div(
          class = "wb-header",
          tags$div(
            class = "wb-header-left",
            tags$span(class = "wb-eyebrow", icon("download"), "Step 6 of 6"),
            tags$span(class = "wb-header-divider"),
            tags$h1(class = "wb-title", "Results Download")
          ),
          tags$ol(
            class = "wb-stepper",
            tags$li(
              class = "wb-step is-done",
              title = "Data Upload",
              tags$span(class = "wb-step-dot", "1"),
              tags$span(class = "wb-step-name", "Upload")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Feature Annotation",
              tags$span(class = "wb-step-dot", "2"),
              tags$span(class = "wb-step-name", "Annotate")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "PCSF Subnetwork",
              tags$span(class = "wb-step-dot", "3"),
              tags$span(class = "wb-step-name", "Network")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Module Identification",
              tags$span(class = "wb-step-dot", "4"),
              tags$span(class = "wb-step-name", "Modules")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-done",
              title = "Function Interpretation",
              tags$span(class = "wb-step-dot", "5"),
              tags$span(class = "wb-step-name", "Function")
            ),
            tags$li(class = "wb-step-line"),
            tags$li(
              class = "wb-step is-current",
              title = "Results Download",
              tags$span(class = "wb-step-dot", "6"),
              tags$span(class = "wb-step-name", "Results")
            )
          )
        ),

        tags$div(
          class = "wb-body",

          # ---------------- Sidebar: what to export ----------------
          tags$div(
            id = "results_export_card",
            class = "wb-side",
            tags$div(
              class = "wb-side-scroll",

              tags$div(
                class = "wb-section",
                tags$div(class = "wb-section-title", icon("table"), "Tables"),
                checkboxGroupInput(
                  "export_items",
                  NULL,
                  choices = c(
                    "Annotation table 1" = "anno1",
                    "Annotation table 2" = "anno2",
                    "Module score"       = "module_score",
                    "Module table"       = "module_table",
                    "Module function"    = "module_function"
                  )
                ),
                selectInput(
                  "export_format", "Table format",
                  choices = c(
                    "Excel (.xlsx)" = "xlsx",
                    "CSV (multiple files)" = "csv",
                    "RData" = "rdata"
                  )
                )
              ),

              tags$div(
                class = "wb-section",
                tags$div(class = "wb-section-title", icon("sitemap"), "Networks"),
                checkboxGroupInput(
                  "network_export_items",
                  NULL,
                  choices = c(
                    "PCSF subnetwork" = "pcsf",
                    "Module network" = "module"
                  )
                ),
                uiOutput("results_module_export_selector"),
                selectInput(
                  "network_export_format", "Network format",
                  choices = c(
                    "Interactive HTML (.html)" = "html",
                    "GraphML (.graphml)" = "graphml"
                  ),
                  selected = "html"
                )
              ),

              uiOutput("results_reports_section")
            ),

            tags$div(
              class = "wb-actions",
              downloadButton(
                "download_results", "Download",
                class = "btn btn-wb-primary"
              ),
              actionButton(
                "back_home_reset", "Back to Home",
                icon = icon("house"), class = "btn btn-wb-next"
              )
            )
          ),

          # ---------------- Work area ----------------
          tags$div(
            id = "results_params_card",
            class = "wb-main",
            tags$div(
              class = "wb-main-inner wb-scroll-area",
              uiOutput("results_overview"),
              uiOutput("analysis_parameters")
            )
          )
        )
      )
    ),

    # Authors Page
    tags$div(
      id = "authors_page",
      style = "display: none;",
      tags$main(
        class = "authors-page",
        tags$section(
          class = "authors-hero-section",
          tags$div(
            class = "authors-hero-inner",
            tags$div(
              tags$p(class = "authors-kicker", icon("users"), "Authors"),
              tags$h1(class = "authors-hero-title", "Project Team"),
              tags$p(
                class = "authors-hero-subtitle",
                "The Shen Lab team at Lee Kong Chian School of Medicine, NTU"
              )
            ),
            tags$div(
              class = "authors-logo-block",
              tags$img(
                src = "shen_lab_footer_logo.png",
                alt = "Shen Lab",
                onerror = "this.onerror=null; this.src='shen_lab_logo6.png';"
              )
            )
          )
        ),

        tags$div(
          class = "authors-body",
          tags$section(
            class = "authors-section",
            tags$div(
              class = "authors-section-head",
              tags$div(
                tags$p(class = "authors-section-kicker", icon("chalkboard-user"), "Faculty"),
                tags$h2(class = "authors-section-title", "Principal Investigators")
              )
            ),
            tags$div(
              class = "faculty-grid",
              tags$article(
                class = "author-card faculty-author",
                tags$div(
                  class = "author-photo faculty-photo",
                  tags$img(
                    src = "authors/xiaotao-shen.jpg",
                    alt = "Xiaotao Shen",
                    onerror = "this.style.display='none'; var fallback=this.nextElementSibling; if (fallback) fallback.style.display='grid';"
                  ),
                  tags$span(class = "author-initials", "XS")
                ),
                tags$div(
                  class = "author-copy",
                  tags$div(class = "author-role-label", icon("user-tie"), "Faculty"),
                  tags$h3(class = "author-name", "Xiaotao Shen"),
                  tags$p(class = "author-role", "Assistant Professor, Lee Kong Chian School of Medicine, NTU"),
                  tags$a(
                    class = "author-email",
                    href = "mailto:xiaotao.shen@ntu.edu.sg",
                    icon("envelope"),
                    tags$span("xiaotao.shen@ntu.edu.sg")
                  )
                )
              ),
              tags$article(
                class = "author-card faculty-author",
                tags$div(
                  class = "author-photo faculty-photo",
                  tags$img(
                    src = "authors/chuchu-wang.jpg",
                    alt = "Chuchu Wang",
                    onerror = "this.style.display='none'; var fallback=this.nextElementSibling; if (fallback) fallback.style.display='grid';"
                  ),
                  tags$span(class = "author-initials", "CW")
                ),
                tags$div(
                  class = "author-copy",
                  tags$div(class = "author-role-label", icon("user-tie"), "Faculty"),
                  tags$h3(class = "author-name", "Chuchu Wang"),
                  tags$p(class = "author-role", "Assistant Professor, Lee Kong Chian School of Medicine, NTU"),
                  tags$a(
                    class = "author-email",
                    href = "mailto:chuchu.wang@ntu.edu.sg",
                    icon("envelope"),
                    tags$span("chuchu.wang@ntu.edu.sg")
                  )
                )
              )
            )
          ),

          tags$section(
            class = "authors-section",
            tags$div(
              class = "authors-section-head",
              tags$div(
                tags$p(class = "authors-section-kicker", icon("graduation-cap"), "Students"),
                tags$h2(class = "authors-section-title", "Research Contributors")
              )
            ),
            tags$div(
              class = "student-grid",
              tags$article(
                class = "author-card student-author",
                tags$div(
                  class = "author-photo student-photo",
                  tags$img(
                    src = "authors/yuting-wang.jpg",
                    alt = "Yuting Wang",
                    onerror = "this.style.display='none'; var fallback=this.nextElementSibling; if (fallback) fallback.style.display='grid';"
                  ),
                  tags$span(class = "author-initials", "YW")
                ),
                tags$div(
                  class = "author-copy",
                  tags$div(class = "author-role-label", icon("microscope"), "Contributor"),
                  tags$h3(class = "author-name", "Yuting Wang"),
                  tags$p(class = "author-role", "PhD Candidate, Lee Kong Chian School of Medicine, NTU"),
                  tags$a(
                    class = "author-email",
                    href = "mailto:yuting012@e.ntu.edu.sg",
                    icon("envelope"),
                    tags$span("yuting012@e.ntu.edu.sg")
                  )
                )
              ),
              tags$article(
                class = "author-card student-author",
                tags$div(
                  class = "author-photo student-photo",
                  tags$img(
                    src = "authors/feifan-zhang.jpg",
                    alt = "Feifan Zhang",
                    onerror = "this.style.display='none'; var fallback=this.nextElementSibling; if (fallback) fallback.style.display='grid';"
                  ),
                  tags$span(class = "author-initials", "FZ")
                ),
                tags$div(
                  class = "author-copy",
                  tags$div(class = "author-role-label", icon("microscope"), "Contributor"),
                  tags$h3(class = "author-name", "Feifan Zhang"),
                  tags$p(class = "author-role", "PhD Candidate, Lee Kong Chian School of Medicine, NTU"),
                  tags$a(
                    class = "author-email",
                    href = "mailto:feifan004@e.ntu.edu.sg",
                    icon("envelope"),
                    tags$span("feifan004@e.ntu.edu.sg")
                  )
                )
              ),
              tags$article(
                class = "author-card student-author",
                tags$div(
                  class = "author-photo student-photo",
                  tags$img(
                    src = "authors/yijiang-liu.jpg",
                    alt = "Yijiang Liu",
                    onerror = "this.style.display='none'; var fallback=this.nextElementSibling; if (fallback) fallback.style.display='grid';"
                  ),
                  tags$span(class = "author-initials", "YL")
                ),
                tags$div(
                  class = "author-copy",
                  tags$div(class = "author-role-label", icon("microscope"), "Contributor"),
                  tags$h3(class = "author-name", "Yijiang Liu"),
                  tags$p(class = "author-role", "PhD Candidate, Lee Kong Chian School of Medicine, NTU"),
                  tags$a(
                    class = "author-email",
                    href = "mailto:liuy0292@e.ntu.edu.sg",
                    icon("envelope"),
                    tags$span("liuy0292@e.ntu.edu.sg")
                  )
                )
              ),
              tags$article(
                class = "author-card student-author",
                tags$div(
                  class = "author-photo student-photo",
                  tags$img(
                    src = "authors/jiramet-kinchagawat.jpg",
                    alt = "Jiramet Kinchagawat",
                    onerror = "this.style.display='none'; var fallback=this.nextElementSibling; if (fallback) fallback.style.display='grid';"
                  ),
                  tags$span(class = "author-initials", "JK")
                ),
                tags$div(
                  class = "author-copy",
                  tags$div(class = "author-role-label", icon("microscope"), "Contributor"),
                  tags$h3(class = "author-name", "Jiramet Kinchagawat"),
                  tags$p(class = "author-role", "PhD Candidate, Lee Kong Chian School of Medicine, NTU"),
                  tags$a(
                    class = "author-email",
                    href = "mailto:jiramet001@e.ntu.edu.sg",
                    icon("envelope"),
                    tags$span("jiramet001@e.ntu.edu.sg")
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Sign In Page
    tags$div(
      id = "signin_page",
      style = "display: none;",
      tags$div(
        class = "auth-page",
        tags$div(
          class = "auth-panel",
          uiOutput("auth_notice"),
          uiOutput("auth_account_panel"),
          uiOutput("auth_form_panel")
        )
      )
    )
  ),

  # Global Footer
  tags$div(
    class = "app-footer",
    tags$div(
      class = "app-footer-inner",
      tags$div(
        tags$img(
          src = "shen_lab_footer_logo.png",
          class = "footer-logo",
          alt = "Shen Lab",
          onerror = "this.onerror=null; this.src='shen_lab_logo6.png';"
        )
      ),
      tags$div(
        class = "footer-links",
        tags$a(
          class = "footer-link",
          onclick = "Shiny.setInputValue('footer_nav', 'home', {priority: 'event'}); return false;",
          "Home"
        ),
        tags$a(
          class = "footer-link",
          onclick = "Shiny.setInputValue('footer_nav', 'home', {priority: 'event'}); setTimeout(function(){ document.querySelector('#home_page .workflow-section')?.scrollIntoView({behavior: 'smooth', block: 'start'}); }, 80); return false;",
          "About"
        ),
        tags$a(
          class = "footer-link",
          href = "https://tidymass.github.io/featuremap_tutorial/",
          target = "_blank",
          rel = "noopener noreferrer",
          "Help"
        ),
        tags$a(
          class = "footer-link",
          href = "https://www.shen-lab.org/contact",
          target = "_blank",
          "Contact Us"
        ),
        tags$a(
          class = "footer-link",
          href = "https://github.com/tidymass/featuremap_shiny",
          target = "_blank",
          "GitHub"
        ),
        tags$a(
          class = "footer-link",
          onclick = "window.scrollTo({top: 0, behavior: 'smooth'}); return false;",
          "Back to top ",
          icon("arrow-up")
        )
      ),
      tags$div(
        class = "footer-meta",
        tags$p(class = "footer-product", "FeatureMAP"),
        tags$p(HTML("&copy; 2026 Shen Lab. All rights reserved.")),
        tags$p("Last updated 2026-08-17"),
        tags$p("Version ", tags$strong("v1.0"))
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
    pcsf_selector_syncing = FALSE,
    pcsf_selector_sync_token = 0L,
    ### Step 4
    module_res    = NULL,
    module_selected_node = NULL,
    module_status = "idle",
    ### Step 5
    function_result = NULL,
    function_status = "idle",
    function_pubmed_used = FALSE,
    function_html_dir = NULL,
    function_debate_html_dir = NULL,
    function_synthesis_html_path = NULL,
    results_export_tables_touched = FALSE,
    results_export_networks_touched = FALSE,
    results_export_tables_updating = FALSE,
    results_export_networks_updating = FALSE,
    ### Account
    auth_user = NULL,
    auth_notice = NULL,
    auth_redirect_after_login = NULL,
    auth_account_return_page = "home",
    auth_mode = "signin",
    auth_reset_token = NULL,
    auth_signup_email = NULL,
    auth_email_delivery_failed = FALSE,
    auth_email_dry_run = FALSE,
    auth_resend_available = FALSE,
    auth_signup_resend_ready_at = 0,
    auth_signin_resend_ready_at = 0,
    auth_reset_request_ready_at = 0,
    auth_query_seen = NULL,
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
  pages <- c("home", "upload", "annotation", "network", "modules", "function",
             "results", "authors", "signin")

  # Steps grouped under the "Analysis" dropdown
  analysis_pages <- c("upload", "annotation", "network", "modules", "function", "results")

  # Pages already converted to the full-bleed workbench layout
  workbench_pages <- c("upload", "annotation", "network", "modules", "function", "results")

  # Analysis pages are protected in the first-stage account system. Public
  # pages stay open so visitors can still read about the tool and team.
  protected_pages <- analysis_pages
  auth_enabled <- featuremap_auth_enabled()

  auth_user_label <- function(user = rv$auth_user) {
    if (is.null(user)) return("Sign In")
    label <- trimws(as.character(user$display_name %||% ""))
    if (!nzchar(label)) label <- trimws(as.character(user$email %||% ""))
    if (!nzchar(label)) label <- "Account"
    label
  }

  auth_user_initials <- function(user = rv$auth_user) {
    label <- auth_user_label(user)
    if (identical(label, "Sign In") || identical(label, "Account")) return(label)
    parts <- unlist(strsplit(label, "[^A-Za-z0-9]+", perl = TRUE))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0 && nzchar(user$email %||% "")) {
      parts <- unlist(strsplit(sub("@.*$", "", user$email), "[^A-Za-z0-9]+", perl = TRUE))
      parts <- parts[nzchar(parts)]
    }
    initials <- paste0(toupper(substr(parts, 1, 1)), collapse = "")
    if (nchar(initials) > 3) initials <- substr(initials, 1, 3)
    if (!nzchar(initials)) initials <- "AC"
    initials
  }

  set_auth_notice <- function(type = c("info", "success", "error"), text) {
    type <- match.arg(type)
    rv$auth_notice <- list(type = type, text = text)
  }

  auth_public_base_url <- function() {
    env_url <- Sys.getenv("FEATUREMAP_PUBLIC_URL", unset = "")
    if (nzchar(trimws(env_url))) return(sub("/+$", "", trimws(env_url)))

    protocol <- session$clientData$url_protocol %||% "http:"
    host <- session$clientData$url_hostname %||% "127.0.0.1"
    port <- session$clientData$url_port %||% ""
    port_part <- if (nzchar(port)) paste0(":", port) else ""
    paste0(protocol, "//", host, port_part)
  }

  auth_cooldown_remaining <- function(key) {
    ready_at <- suppressWarnings(as.numeric(rv[[key]] %||% 0))
    if (is.na(ready_at)) ready_at <- 0
    max(0L, ceiling(ready_at - as.numeric(Sys.time())))
  }

  auth_set_cooldown <- function(key, seconds = 60) {
    rv[[key]] <- as.numeric(Sys.time()) + seconds
  }

  auth_cooldown_blocked <- function(key, label = "another email") {
    remaining <- auth_cooldown_remaining(key)
    if (remaining <= 0) return(FALSE)
    set_auth_notice("info", sprintf("Please wait %s seconds before requesting %s.", remaining, label))
    TRUE
  }

  auth_cooldown_button <- function(input_id, label, cooldown_key, class = "btn btn-auth-secondary") {
    remaining <- auth_cooldown_remaining(cooldown_key)
    is_waiting <- remaining > 0
    actionButton(
      input_id,
      if (is_waiting) sprintf("%s (%ss)", label, remaining) else label,
      class = class,
      onclick = auth_button_press_js,
      disabled = if (is_waiting) "disabled" else NULL
    )
  }

  auth_start_button_cooldown <- function(input_id, label, seconds = 60) {
    shinyjs::delay(
      40,
      shinyjs::runjs(sprintf(
        "window.FeatureMAPAuthCooldown && window.FeatureMAPAuthCooldown(%s, %s, %d);",
        jsonlite::toJSON(input_id, auto_unbox = TRUE),
        jsonlite::toJSON(label, auto_unbox = TRUE),
        as.integer(seconds)
      ))
    )
  }

  auth_return_page <- function() {
    page <- rv$auth_account_return_page %||% "home"
    if (!(page %in% pages) || identical(page, "signin")) page <- "home"
    page
  }

  output$authenticated <- reactive({
    !auth_enabled || !is.null(rv$auth_user)
  })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  output$auth_nav <- renderUI({
    if (!auth_enabled) return(NULL)

    if (is.null(rv$auth_user)) {
      return(actionLink(
        "nav_signin",
        tagList(icon("right-to-bracket"), tags$span("Sign In")),
        class = "nav-link nav-link-signin"
      ))
    }

    tagList(
      actionLink(
        "nav_signin",
        tags$span(class = "nav-auth-user", auth_user_initials()),
        class = "nav-link nav-link-signin",
        title = auth_user_label()
      ),
      actionLink(
        "auth_logout_nav",
        icon("arrow-right-from-bracket"),
        class = "nav-auth-logout",
        title = "Log out",
        `aria-label` = "Log out"
      )
    )
  })
  outputOptions(output, "auth_nav", suspendWhenHidden = FALSE)

  output$auth_notice <- renderUI({
    if (!auth_enabled) return(NULL)

    notice <- rv$auth_notice
    if (is.null(notice) || !nzchar(notice$text %||% "")) return(NULL)
    icon_name <- switch(
      notice$type,
      success = "circle-check",
      error = "circle-exclamation",
      "circle-info"
    )
    tags$div(
      class = paste("auth-alert", paste0("auth-alert-", notice$type)),
      icon(icon_name),
      tags$span(notice$text)
    )
  })

  output$auth_account_panel <- renderUI({
    if (!auth_enabled) return(NULL)

    user <- rv$auth_user
    if (is.null(user)) return(NULL)

    tags$div(
      class = "auth-account",
      tags$div(class = "auth-account-mark", icon("user-check")),
      tags$div(
        tags$h2(class = "auth-account-title", paste("Signed in as", auth_user_label(user))),
        tags$p(class = "auth-account-meta", user$email %||% "")
      ),
      tags$div(
        class = "auth-account-actions",
        actionButton(
          "auth_continue",
          tagList(icon("play"), "Continue"),
          class = "btn btn-auth-primary",
          onclick = auth_button_press_js
        ),
        actionButton(
          "auth_logout",
          tagList(icon("arrow-right-from-bracket"), "Log Out"),
          class = "btn btn-auth-secondary",
          onclick = auth_button_press_js
        )
      )
    )
  })

  output$auth_form_panel <- renderUI({
    if (!auth_enabled) {
      return(tagList(
        tags$h1(class = "auth-title", "Sign in"),
        tags$p(class = "auth-switch", "Sign in is disabled for this local session.")
      ))
    }

    if (!is.null(rv$auth_user)) return(NULL)

    mode <- rv$auth_mode %||% "signin"
    if (identical(mode, "signup_success")) {
      delivery_failed <- isTRUE(rv$auth_email_delivery_failed)
      dry_run <- isTRUE(rv$auth_email_dry_run)
      return(tags$div(
        class = "auth-success",
        tags$div(class = "auth-success-mark", icon("envelope")),
        tags$h1(
          class = "auth-success-title",
          if (dry_run) "Dry run enabled" else if (delivery_failed) "Account created" else "Check your email"
        ),
        tags$p(
          class = "auth-success-text",
          if (dry_run) "No email was sent to " else if (delivery_failed) "Verification email could not be sent to " else "Verification link sent to ",
          tags$span(class = "auth-success-email", rv$auth_signup_email %||% "your email"),
          "."
        ),
        if (dry_run) {
          tags$p(class = "auth-success-text", "Restart without FEATUREMAP_EMAIL_DRY_RUN=true to send real email.")
        },
        tags$div(
          class = "auth-success-actions",
          auth_cooldown_button(
            "resend_signup_verification",
            "Resend email",
            "auth_signup_resend_ready_at",
            class = "btn btn-auth-secondary"
          ),
          actionButton(
            "auth_success_signin",
            "Back to sign in",
            class = "btn btn-auth-primary",
            onclick = auth_button_press_js
          )
        )
      ))
    }

    if (identical(mode, "forgot")) {
      return(tagList(
        tags$h1(class = "auth-title", "Reset password"),
        tags$div(
          class = "auth-form",
          auth_text_field("forgot_email", "Email", "envelope", type = "email", autocomplete = "email", required = TRUE),
          auth_cooldown_button(
            "forgot_submit",
            "Send reset link",
            "auth_reset_request_ready_at",
            class = "btn btn-auth-primary"
          )
        ),
        tags$p(
          class = "auth-switch",
          actionLink("auth_show_signin", "Back to sign in", class = "auth-switch-link")
        )
      ))
    }

    if (identical(mode, "reset")) {
      return(tagList(
        tags$h1(class = "auth-title", "Reset password"),
        tags$div(
          class = "auth-form",
          auth_password_field(
            "reset_password",
            "New password",
            autocomplete = "new-password",
            required = TRUE,
            hint = "8+ chars, covering at least 3 of uppercase, lowercase, number, symbol."
          ),
          auth_password_field("reset_password_confirm", "Confirm password", autocomplete = "new-password", required = TRUE),
          actionButton(
            "reset_submit",
            "Update password",
            class = "btn btn-auth-primary",
            onclick = auth_button_press_js
          )
        ),
        tags$p(
          class = "auth-switch",
          actionLink("auth_show_signin", "Back to sign in", class = "auth-switch-link")
        )
      ))
    }

    if (identical(mode, "signup")) {
      return(tagList(
        tags$h1(class = "auth-title", "Sign up"),
        tags$div(
          class = "auth-form",
          auth_text_field("signup_name", "Display name", "user", autocomplete = "name", required = TRUE),
          auth_text_field("signup_email", "Email", "envelope", type = "email", autocomplete = "email", required = TRUE),
          auth_text_field("signup_institution", "Institution", "university", autocomplete = "organization", required = TRUE),
          auth_text_field("signup_country", "Country/Region", "globe", required = TRUE),
          auth_password_field(
            "signup_password",
            "Password",
            autocomplete = "new-password",
            required = TRUE,
            hint = "8+ chars, covering at least 3 of uppercase, lowercase, number, symbol."
          ),
          auth_password_field("signup_password_confirm", "Confirm password", autocomplete = "new-password", required = TRUE),
          actionButton(
            "signup_submit",
            "Create account",
            class = "btn btn-auth-primary",
            onclick = auth_button_press_js
          )
        ),
        tags$p(
          class = "auth-switch",
          "Already have an account? ",
          actionLink("auth_show_signin", "Sign in", class = "auth-switch-link")
        )
      ))
    }

    tagList(
      tags$h1(class = "auth-title", "Sign in"),
      tags$div(
        class = "auth-form",
        auth_text_field("signin_email", "Email", "envelope", type = "email", autocomplete = "email", required = TRUE),
        auth_password_field(
          "signin_password",
          tags$span(
            class = "auth-label-row",
            auth_required_label("Password", TRUE),
            actionLink("auth_show_forgot", "Forgot password?", class = "auth-inline-link")
          ),
          autocomplete = "current-password",
          required = TRUE,
          label_required = FALSE
        ),
        actionButton(
          "signin_submit",
          "Sign in",
          class = "btn btn-auth-primary",
          onclick = auth_button_press_js
        ),
        if (isTRUE(rv$auth_resend_available)) {
          auth_cooldown_button(
            "resend_verification",
            "Resend verification email",
            "auth_signin_resend_ready_at",
            class = "btn btn-auth-secondary"
          )
        }
      ),
      tags$p(
        class = "auth-switch",
        "No account yet? ",
        actionLink("auth_show_signup", "Sign up", class = "auth-switch-link")
      )
    )
  })
  outputOptions(output, "auth_form_panel", suspendWhenHidden = FALSE)
  
  # Function to switch pages.
  # Everything is done in a single browser call: the old version sent one
  # websocket message per page and per nav link (~22 round trips), which is
  # what made switching feel sluggish.
  switch_page <- function(page) {
    if (!(page %in% pages)) page <- "home"
    if (auth_enabled && page %in% protected_pages && is.null(rv$auth_user)) {
      rv$auth_redirect_after_login <- page
      set_auth_notice("info", "Sign in or create an account to continue to the analysis workflow.")
      page <- "signin"
    }

    rv$current_page <- page

    js_pages <- paste0("['", paste(pages, collapse = "','"), "']")

    shinyjs::runjs(sprintf(
      "(function() {
         var target = '%s';
         %s.forEach(function(p) {
           var el = document.getElementById(p + '_page');
           if (el) { el.style.display = (p === target) ? '' : 'none'; }
           var nav = document.getElementById('nav_' + p);
           if (nav) { nav.classList.toggle('active', p === target); }
         });
         var an = document.getElementById('nav_analysis');
         if (an) { an.classList.toggle('active', %s); }
         document.body.classList.toggle('fm-workbench', %s);
         if (window.closeAnalysisMenu) { window.closeAnalysisMenu(); }
         if (window.setFeaturemapNavHeight) { window.setFeaturemapNavHeight(); }
         setTimeout(function() {
           window.dispatchEvent(new Event('resize'));
           if (window.forcePlotlyResize) { window.forcePlotlyResize(); }
         }, 120);
       })();",
      page,
      js_pages,
      if (page %in% analysis_pages) "true" else "false",
      if (page %in% workbench_pages) "true" else "false"
    ))
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

    # Put every parameter panel back to the values it had on a fresh load
    # (numeric/select/slider/checkbox/text/password fields and file pickers).
    for (card in c("upload_param_card", "annotation_param_card",
                   "network_param_card", "modules_param_card",
                   "function_param_card", "results_export_card")) {
      shinyjs::reset(card)
    }
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
        list(target = "function_param_card", title = "Step 5 Guide: Parameters", text = "Set functional interpretation options and provide required API key(s) in this panel."),
        list(target = "run_function_annotation", title = "Step 5 Guide: Run", text = "Click this button to run AI-powered functional interpretation."),
        list(target = "function_result_card", title = "Step 5 Guide: Result", text = "Check functional interpretation outputs in this result panel.")
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
  
  # One card for the whole run: it opens in the running state and is
  # switched to the success (or error) state in place, so the two never
  # overlap. It stays open until the user dismisses it.
  # Shared row for the card body (defined before the builders that use it).
  proc_row <- function(icon_name, label, value) {
    tags$div(
      class = "wb-done-row",
      tags$span(class = "wb-done-row-label", icon(icon_name), label),
      tags$span(class = "wb-done-row-value", value)
    )
  }

  # One card for the whole run. All three faces (running / success / error)
  # are rendered into the modal up front and swapped by a class, so the card
  # can never replay a previous step's state -- which is what happened when
  # the body was a uiOutput and Shiny replayed its last cached value on
  # re-binding.
  proc_modal_body <- function(state, title, text, rows = NULL) {
    tags$div(
      id = "fm_proc_card",
      class = paste0("fm-process-modal is-", state),
      tags$div(class = "fm-modal-icon running", icon("spinner", class = "fa-spin")),
      HTML(
        paste0(
          "<svg class='wb-check' viewBox='0 0 52 52'>",
          "<circle class='wb-check-halo' cx='26' cy='26' r='24'/>",
          "<circle class='wb-check-ring' cx='26' cy='26' r='24'/>",
          "<path class='wb-check-mark' d='M15 26.5 l7.5 7.5 l14.5 -15.5'/>",
          "</svg>"
        )
      ),
      tags$div(class = "fm-modal-icon error", icon("times")),
      tags$div(id = "fm_proc_title", class = "fm-modal-title", title),
      tags$div(id = "fm_proc_text", class = "fm-modal-text", text),
      tags$div(id = "fm_proc_rows", class = "wb-done-rows",
               if (!is.null(rows) && length(rows) > 0) rows else NULL),
      tags$div(
        class = "fm-modal-actions",
        actionButton("process_modal_close", "Close", class = "btn btn-wb-primary")
      )
    )
  }

  open_process_modal <- function(state, title, text, rows = NULL) {
    removeModal()
    showModal(
      modalDialog(
        easyClose = FALSE,
        footer = NULL,
        proc_modal_body(state, title, text, rows)
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

  # Switch the already-open card to another state, in place.
  set_process_state <- function(state, title, text, rows = NULL) {
    rows_html <- if (!is.null(rows) && length(rows) > 0) as.character(tagList(rows)) else ""
    shinyjs::runjs(sprintf(
      "(function() {
         var c = document.getElementById('fm_proc_card');
         if (!c) return;
         var title = document.getElementById('fm_proc_title');
         var text = document.getElementById('fm_proc_text');
         var rows = document.getElementById('fm_proc_rows');
         if (title) title.textContent = %s;
         if (text) text.textContent = %s;
         if (rows) rows.innerHTML = %s;
         c.className = 'fm-process-modal is-%s';
       })();",
      jsonlite::toJSON(title %||% "", auto_unbox = TRUE),
      jsonlite::toJSON(text %||% "", auto_unbox = TRUE),
      jsonlite::toJSON(rows_html, auto_unbox = TRUE),
      state
    ))
  }

  show_process_modal <- function(title, text) {
    open_process_modal("running", title, text)
  }

  update_process_modal <- function(status = c("success", "error"), title, text) {
    status <- match.arg(status)
    set_process_state(status, title, text)
  }

  # Switch the already-open card to its success state and keep it there.
  finish_process_modal <- function(title, text, rows = NULL) {
    set_process_state("success", title, text, rows)
  }

  # For steps with no running phase (Data Upload): open the same card
  # straight in its success state.
  show_result_modal <- function(title, text, rows = NULL) {
    open_process_modal("success", title, text, rows)
  }

  observeEvent(input$process_modal_close, {
    removeModal()
  })

  close_process_modal <- function(delay_ms = 900) {
    shinyjs::delay(delay_ms, removeModal())
  }

  # Corner toasts were removed; anything the user must see uses the same
  # centred card as the running/failed states.
  show_error_modal <- function(title, text) {
    open_process_modal("error", title, text)
    close_process_modal(2600)
  }

  post_auth_target <- function() {
    rv$auth_redirect_after_login <- NULL
    "home"
  }

  require_auth <- function(target_page = "upload") {
    if (!auth_enabled) return(TRUE)
    if (!is.null(rv$auth_user)) return(TRUE)
    rv$auth_redirect_after_login <- target_page
    set_auth_notice("info", "Sign in or create an account to continue to the analysis workflow.")
    switch_page("signin")
    FALSE
  }

  observe({
    if (!auth_enabled) return(invisible(NULL))
    search <- session$clientData$url_search %||% ""
    if (identical(search, rv$auth_query_seen)) return(invisible(NULL))
    rv$auth_query_seen <- search

    params <- parseQueryString(search)
    if (nzchar(params$verify_token %||% "")) {
      result <- tryCatch(
        featuremap_auth_verify_email(params$verify_token),
        error = function(e) e
      )
      rv$auth_mode <- "signin"
      rv$auth_resend_available <- FALSE
      if (inherits(result, "error")) {
        set_auth_notice("error", conditionMessage(result))
      } else {
        set_auth_notice("success", "Email verified. You can sign in now.")
      }
      shinyjs::runjs("if (window.history && window.history.replaceState) window.history.replaceState({}, document.title, window.location.pathname);")
      switch_page("signin")
    } else if (nzchar(params$reset_token %||% "")) {
      rv$auth_reset_token <- params$reset_token
      rv$auth_mode <- "reset"
      rv$auth_resend_available <- FALSE
      rv$auth_notice <- NULL
      shinyjs::runjs("if (window.history && window.history.replaceState) window.history.replaceState({}, document.title, window.location.pathname);")
      switch_page("signin")
    }
  })

  logout_user <- function() {
    rv$auth_user <- NULL
    rv$auth_redirect_after_login <- NULL
    set_auth_notice("success", "You have signed out.")
    switch_page("home")
  }

  observeEvent(input$auth_show_signup, {
    rv$auth_mode <- "signup"
    rv$auth_notice <- NULL
    rv$auth_resend_available <- FALSE
    rv$auth_signup_email <- NULL
    rv$auth_email_delivery_failed <- FALSE
    rv$auth_email_dry_run <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$auth_show_signin, {
    rv$auth_mode <- "signin"
    rv$auth_notice <- NULL
    rv$auth_reset_token <- NULL
    rv$auth_resend_available <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$auth_success_signin, {
    rv$auth_mode <- "signin"
    rv$auth_notice <- NULL
    rv$auth_signup_email <- NULL
    rv$auth_email_delivery_failed <- FALSE
    rv$auth_email_dry_run <- FALSE
    rv$auth_resend_available <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$auth_show_forgot, {
    rv$auth_mode <- "forgot"
    rv$auth_notice <- NULL
    rv$auth_resend_available <- FALSE
    rv$auth_email_delivery_failed <- FALSE
    rv$auth_email_dry_run <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$signin_submit, {
    if (!auth_enabled) return(invisible(NULL))
    rv$auth_resend_available <- FALSE

    result <- tryCatch(
      featuremap_auth_login(input$signin_email, input$signin_password),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      if (grepl("verify your email", conditionMessage(result), ignore.case = TRUE)) {
        rv$auth_resend_available <- TRUE
      }
      set_auth_notice("error", conditionMessage(result))
      return(invisible(NULL))
    }

    rv$auth_user <- result
    set_auth_notice("success", "Signed in.")
    switch_page(post_auth_target())
  })

  observeEvent(input$signup_submit, {
    if (!auth_enabled) return(invisible(NULL))

    password <- as.character(input$signup_password %||% "")
    password_confirm <- as.character(input$signup_password_confirm %||% "")

    if (!identical(password, password_confirm)) {
      set_auth_notice("error", "Passwords do not match.")
      return(invisible(NULL))
    }

    result <- tryCatch(
      featuremap_auth_signup(
        display_name = input$signup_name,
        email = input$signup_email,
        password = password,
        institution = input$signup_institution,
        country_region = input$signup_country
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      set_auth_notice("error", conditionMessage(result))
      return(invisible(NULL))
    }

    mail_result <- tryCatch(
      featuremap_auth_send_verification_email(result$email, result$verification_token, auth_public_base_url()),
      error = function(e) e
    )

    rv$auth_signup_email <- result$email
    rv$auth_mode <- "signup_success"
    rv$auth_resend_available <- FALSE
    auth_set_cooldown("auth_signup_resend_ready_at", 60)
    if (inherits(mail_result, "error")) {
      rv$auth_email_delivery_failed <- TRUE
      rv$auth_email_dry_run <- FALSE
      set_auth_notice("error", conditionMessage(mail_result))
    } else {
      rv$auth_email_delivery_failed <- FALSE
      rv$auth_email_dry_run <- isTRUE(mail_result$dry_run)
      if (isTRUE(mail_result$dry_run)) {
        set_auth_notice("info", "Email dry-run is enabled. No real email was sent.")
      } else {
        rv$auth_notice <- NULL
      }
    }
    switch_page("signin")
    auth_start_button_cooldown("resend_signup_verification", "Resend email", 60)
  })

  observeEvent(input$resend_signup_verification, {
    if (!auth_enabled) return(invisible(NULL))

    email <- featuremap_auth_normalize_email(rv$auth_signup_email)
    if (!featuremap_auth_validate_email(email)) {
      set_auth_notice("error", "Enter your email first.")
      return(invisible(NULL))
    }
    if (auth_cooldown_blocked("auth_signup_resend_ready_at", "another verification email")) {
      return(invisible(NULL))
    }

    result <- tryCatch(
      featuremap_auth_resend_verification(email),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      set_auth_notice("error", conditionMessage(result))
      return(invisible(NULL))
    }
    if (is.null(result)) {
      rv$auth_email_delivery_failed <- FALSE
      rv$auth_email_dry_run <- FALSE
      auth_set_cooldown("auth_signup_resend_ready_at", 60)
      set_auth_notice("success", "If that email still needs verification, a new email has been sent.")
      auth_start_button_cooldown("resend_signup_verification", "Resend email", 60)
      return(invisible(NULL))
    }

    mail_result <- tryCatch(
      featuremap_auth_send_verification_email(result$email, result$verification_token, auth_public_base_url()),
      error = function(e) e
    )
    if (inherits(mail_result, "error")) {
      set_auth_notice("error", conditionMessage(mail_result))
      return(invisible(NULL))
    }
    rv$auth_email_delivery_failed <- FALSE
    rv$auth_email_dry_run <- isTRUE(mail_result$dry_run)
    auth_set_cooldown("auth_signup_resend_ready_at", 60)
    auth_start_button_cooldown("resend_signup_verification", "Resend email", 60)
    if (isTRUE(mail_result$dry_run)) {
      set_auth_notice("info", "Email dry-run is enabled. No real email was sent.")
    } else {
      set_auth_notice("success", "Verification email sent.")
    }
  })

  observeEvent(input$resend_verification, {
    if (!auth_enabled) return(invisible(NULL))

    email <- featuremap_auth_normalize_email(input$signin_email)
    if (!featuremap_auth_validate_email(email)) {
      set_auth_notice("error", "Enter your email first.")
      return(invisible(NULL))
    }
    if (auth_cooldown_blocked("auth_signin_resend_ready_at", "another verification email")) {
      return(invisible(NULL))
    }

    result <- tryCatch(
      featuremap_auth_resend_verification(email),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      set_auth_notice("error", conditionMessage(result))
      return(invisible(NULL))
    }
    if (is.null(result)) {
      auth_set_cooldown("auth_signin_resend_ready_at", 60)
      set_auth_notice("success", "If that email exists and is not verified yet, a new verification email has been sent.")
      rv$auth_resend_available <- FALSE
      auth_start_button_cooldown("resend_verification", "Resend verification email", 60)
      return(invisible(NULL))
    }

    mail_result <- tryCatch(
      featuremap_auth_send_verification_email(result$email, result$verification_token, auth_public_base_url()),
      error = function(e) e
    )
    if (inherits(mail_result, "error")) {
      set_auth_notice("error", conditionMessage(mail_result))
      return(invisible(NULL))
    }

    if (isTRUE(mail_result$dry_run)) {
      set_auth_notice("info", "Email dry-run is enabled. No real email was sent.")
    } else {
      set_auth_notice("success", "If that email exists and is not verified yet, a new verification email has been sent.")
    }
    auth_set_cooldown("auth_signin_resend_ready_at", 60)
    rv$auth_resend_available <- TRUE
    auth_start_button_cooldown("resend_verification", "Resend verification email", 60)
  })

  observeEvent(input$forgot_submit, {
    if (!auth_enabled) return(invisible(NULL))

    email <- featuremap_auth_normalize_email(input$forgot_email)
    if (!featuremap_auth_validate_email(email)) {
      set_auth_notice("error", "Enter your email first.")
      return(invisible(NULL))
    }
    if (auth_cooldown_blocked("auth_reset_request_ready_at", "another password reset email")) {
      return(invisible(NULL))
    }

    result <- tryCatch(
      featuremap_auth_request_password_reset(email),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      set_auth_notice("error", conditionMessage(result))
      return(invisible(NULL))
    }
    if (!is.null(result)) {
      mail_result <- tryCatch(
        featuremap_auth_send_password_reset_email(result$email, result$reset_token, auth_public_base_url()),
        error = function(e) e
      )
      if (inherits(mail_result, "error")) {
        set_auth_notice("error", conditionMessage(mail_result))
        return(invisible(NULL))
      }
    }
    auth_set_cooldown("auth_reset_request_ready_at", 60)
    rv$auth_mode <- "signin"
    set_auth_notice("success", "If that email has an account, a password reset link has been sent.")
  })

  observeEvent(input$reset_submit, {
    if (!auth_enabled) return(invisible(NULL))

    password <- as.character(input$reset_password %||% "")
    password_confirm <- as.character(input$reset_password_confirm %||% "")
    if (!identical(password, password_confirm)) {
      set_auth_notice("error", "Passwords do not match.")
      return(invisible(NULL))
    }

    result <- tryCatch(
      featuremap_auth_reset_password(rv$auth_reset_token, password),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      set_auth_notice("error", conditionMessage(result))
      return(invisible(NULL))
    }

    rv$auth_reset_token <- NULL
    rv$auth_mode <- "signin"
    set_auth_notice("success", "Password updated. You can sign in now.")
  })

  observeEvent(input$auth_continue, {
    switch_page(auth_return_page())
  })

  observeEvent(input$auth_logout, {
    logout_user()
  })

  observeEvent(input$auth_logout_nav, {
    logout_user()
  })
  
  # Navigation observers
  observeEvent(input$nav_home, { switch_page("home") })
  observeEvent(input$nav_upload, { switch_page("upload") })
  observeEvent(input$nav_annotation, { switch_page("annotation") })
  observeEvent(input$nav_network, { switch_page("network") })
  observeEvent(input$nav_modules, { switch_page("modules") })
  observeEvent(input$nav_function, { switch_page("function") })
  observeEvent(input$nav_results, { switch_page("results") })
  observeEvent(input$nav_authors, { switch_page("authors") })
  observeEvent(input$nav_signin, {
    if (!auth_enabled) {
      switch_page("home")
      return(invisible(NULL))
    }
    if (is.null(rv$auth_user)) {
      rv$auth_mode <- "signin"
    } else {
      current_page <- rv$current_page %||% "home"
      if (current_page %in% pages && !identical(current_page, "signin")) {
        rv$auth_account_return_page <- current_page
      }
    }
    switch_page("signin")
  })

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

  observeEvent(input$footer_nav, {
    target <- input$footer_nav
    if (target %in% c("home", "upload", "annotation", "network", "modules", "function", "results")) {
      switch_page(target)
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
    if (!require_auth("upload")) return(invisible(NULL))

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
        load("reference/metabolic_reaction_network.rda", envir = e_mrn)
        mrn_obj <- e_mrn[[ls(e_mrn)[1]]]

        e_ref <- new.env()
        load("reference/featuremap_compound.rda", envir = e_ref)
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

    show_result_modal(
      "Data loaded successfully",
      "You can continue to Feature Annotation.",
      list(
        proc_row("table", "Feature table",
                 paste0(format(nrow(loaded$feature_tbl), big.mark = ","), " rows")),
        proc_row("vials", "MS2 table",
                 if (is.null(loaded$ms2_tbl)) "not provided"
                 else paste0(format(nrow(loaded$ms2_tbl), big.mark = ","), " rows"))
      )
    )
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
      load("reference/demo_feature_table.rda", envir = e_feat)
      feature_tbl <- e_feat[[ls(e_feat)[1]]]
      check_feature_columns(feature_tbl)

      e_ms2 <- new.env()
      load("reference/demo_ms2.rda", envir = e_ms2)
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
      show_error_modal("Demo data unavailable", paste("Demo prepare failed:", staged$message))
      return()
    }

    rv$staged_feature_data <- staged$feature_tbl
    rv$staged_ms2_data <- staged$ms2_tbl

    rv$upload_note <- "Demo data is prepared. Click Load to continue."
    rv$upload_note_type <- "info"

    # Show the staged demo files in the two upload fields rather than
    # popping a notification in the corner.
    shinyjs::runjs(
      paste0(
        "(function() {",
        "  function setFileName(id, name) {",
        "    var el = document.getElementById(id);",
        "    if (!el) { return; }",
        "    var wrap = el.closest('.input-group') || el.closest('.form-group');",
        "    if (!wrap) { return; }",
        "    var box = wrap.querySelector('input[type=text]');",
        "    if (box) { box.value = name; box.classList.add('wb-file-demo'); }",
        "  }",
        "  setFileName('feature_file', 'demo_feature_table.rda');",
        "  setFileName('ms2_file', 'demo_ms2.rda');",
        "})();"
      )
    )
  })
  
  
  
  # Shown over the preview area until anything is loaded, so the page
  # explains what it wants instead of sitting empty.
  output$upload_empty_state <- renderUI({
    if (!is.null(rv$feature_data)) {
      return(NULL)
    }

    empty_step <- function(num, text, tag = NULL, tag_class = NULL) {
      tags$div(
        class = "wb-empty-step",
        tags$span(class = "wb-empty-step-num", num),
        tags$span(
          class = "wb-empty-step-text",
          HTML(text),
          if (!is.null(tag)) {
            tags$span(class = paste("wb-empty-tag", tag_class), tag)
          }
        )
      )
    }

    tags$div(
      class = "wb-empty-layer",
      tags$div(
        class = "wb-empty",
        tags$div(class = "wb-empty-icon", icon("file-arrow-up")),
        tags$div(class = "wb-empty-title", "Start by loading your data"),
        tags$div(
          class = "wb-empty-steps",
          empty_step(
            "1",
            "Choose a <b>feature table</b> with m/z, RT, polarity and phenotype association",
            "required", "is-required"
          ),
          empty_step(
            "2",
            "Add an <b>MS2 annotation table</b> to raise annotation confidence",
            "optional", "is-optional"
          ),
          empty_step(
            "3",
            "Keep the <b>default reference</b>, or upload your own database and reaction network"
          ),
          empty_step(
            "4",
            "Click <b>Load</b> to validate the input and preview it here"
          )
        ),
        tags$p(
          class = "wb-empty-foot",
          "No data at hand? Try ", tags$b("Use Demo Data"), " to run the full pipeline."
        )
      )
    )
  })


  output$feature_preview <- renderDT({
    if (is.null(rv$feature_data)) {
      return(render_table_message("No feature table loaded yet"))
    }
    wb_preview_table(rv$feature_data)
  })

  output$download_demo_data <- downloadHandler(
    filename = function() {
      paste0("featuremap_demo_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      resolve_demo_file <- function(filename) {
        candidate_paths <- c(
          system.file(file.path("app", "reference", filename), package = "featuremapshiny"),
          system.file(file.path("reference", filename), package = "featuremapshiny"),
          file.path("reference", filename),
          file.path("inst", "app", "reference", filename)
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

      # Flatten with zip -j instead of setwd(): the working directory is
      # process-wide and shared by every concurrent Shiny session.
      zip_flat(file, demo_files)
    }
  )

  output$ms2_preview <- renderDT({
    if (is.null(rv$ms2_data)) {
      return(render_table_message("No MS2 table loaded yet"))
    }
    wb_preview_table(rv$ms2_data)
  })

  render_table_message <- function(msg) {
    datatable(
      data.frame(msg = msg),
      options = list(dom = "t", ordering = FALSE),
      class = "wb-dt-empty",
      colnames = "",
      rownames = FALSE
    )
  }

  # Shared empty state: icon, headline and a numbered list of what to do.
  wb_empty_state <- function(icon_name, title, steps, foot = NULL) {
    tags$div(
      class = "wb-empty-layer",
      tags$div(
        class = "wb-empty",
        tags$div(class = "wb-empty-icon", icon(icon_name)),
        tags$div(class = "wb-empty-title", title),
        tags$div(class = "wb-empty-steps", steps),
        if (!is.null(foot)) tags$p(class = "wb-empty-foot", foot)
      )
    )
  }

  wb_empty_step <- function(num, text, tag = NULL, tag_class = NULL) {
    tags$div(
      class = "wb-empty-step",
      tags$span(class = "wb-empty-step-num", num),
      tags$span(
        class = "wb-empty-step-text",
        HTML(text),
        if (!is.null(tag)) tags$span(class = paste("wb-empty-tag", tag_class), tag)
      )
    )
  }

  # Shared chrome for every preview table: search on top, rows-per-page and
  # pagination on the bottom, no "Showing x to y of z" line.
  wb_preview_table <- function(df) {
    show_signif <- vapply(
      df,
      function(x) {
        is.numeric(x) && !is.integer(x) &&
          any(is.finite(x) & (x %% 1 != 0))
      },
      logical(1)
    )

    dt <- datatable(
      df,
      rownames = FALSE,
      class = "row-border",
      options = list(
        pageLength = 15,
        lengthMenu = c(15, 25, 50, 100),
        pagingType = "simple",
        dom = "r<'wb-dt-scroll't><'wb-dt-bottom'lp>",
        ordering = FALSE,
        autoWidth = FALSE,
        language = list(
          lengthMenu = "Rows _MENU_",
          paginate = list(previous = "Prev", `next` = "Next")
        ),
        # DataTables has no "jump to page" control, so build one between
        # the Prev/Next buttons and keep it in sync on every redraw.
        drawCallback = DT::JS(
          "function(settings) {",
          "  var api = this.api();",
          "  var info = api.page.info();",
          "  var pag = $(api.table().container()).find('.dataTables_paginate');",
          "  if (pag.length === 0) { return; }",
          "  var host = pag.find('ul.pagination');",
          "  if (host.length === 0) { host = pag; }",
          "  var box = host.find('.wb-page-jump');",
          "  if (box.length === 0) {",
          "    box = $('<li class=\"wb-page-jump\"><input type=\"number\" class=\"wb-page-input\" min=\"1\"><span class=\"wb-page-total\"></span></li>');",
          "    var nextBtn = host.find('.next');",
          "    if (nextBtn.length) { nextBtn.before(box); } else { host.append(box); }",
          "    box.find('.wb-page-input').on('change keyup', function(ev) {",
          "      if (ev.type === 'keyup' && ev.key !== 'Enter') { return; }",
          "      var total = api.page.info().pages;",
          "      var v = parseInt(this.value, 10);",
          "      if (isNaN(v) || v < 1) { v = 1; }",
          "      if (v > total) { v = total; }",
          "      this.value = v;",
          "      api.page(v - 1).draw('page');",
          "    });",
          "  }",
          "  box.find('.wb-page-input').attr('max', info.pages).val(info.page + 1);",
          "  box.find('.wb-page-total').text('of ' + info.pages);",
          "}"
        )
      )
    )

    if (any(show_signif)) {
      dt <- DT::formatSignif(dt, columns = names(df)[show_signif], digits = 4)
    }

    dt
  }
  
  observeEvent(input$next_to_annotation, {
    if (!require_auth("annotation")) return(invisible(NULL))

    req(!is.null(rv$feature_data), !is.null(rv$ref_database), !is.null(rv$mrn))
    switch_page("annotation")
  })
  
  ## Step 2 Feature annotation====================================
  observeEvent(input$run_annotation, {
    if (!require_auth("annotation")) return(invisible(NULL))
    
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
        update_process_modal("error", "Step 2 Failed", res$message)
        close_process_modal(1800)
        shinyjs::enable("run_annotation")
      } else {
        rv$annotation_object  <- res
        rv$annotated_table    <- res$anno_feature_table1
        rv$annotation_summary <- res$anno_redundancy
        rv$annotation_status  <- "finished"

        tbl <- res$anno_feature_table1
        n_entries <- if (is.data.frame(tbl)) nrow(tbl) else 0L
        feature_col <- if (is.data.frame(tbl)) {
          pick_first_col(tbl, c("variable_id", "feature_id", "feature", "feature_name", "id"))
        } else NULL
        n_annotated <- if (!is.null(feature_col)) {
          length(unique(stats::na.omit(tbl[[feature_col]])))
        } else n_entries
        n_total <- if (!is.null(rv$feature_data)) nrow(rv$feature_data) else NA_integer_
        annotated_txt <- paste0(
          format(n_annotated, big.mark = ","),
          if (!is.na(n_total)) {
            paste0(" / ", format(n_total, big.mark = ","),
                   if (n_total > 0) sprintf("  (%.1f%%)", 100 * n_annotated / n_total) else "")
          } else ""
        )

        finish_process_modal(
          "Annotation finished",
          "You can continue to PCSF Subnetwork.",
          list(
            proc_row("bullseye", "Annotated features", annotated_txt),
            proc_row("link", "Feature-metabolite pairs", format(n_entries, big.mark = ","))
          )
        )
        
        shinyjs::enable("run_annotation")
      }
      
      shinyjs::enable("run_annotation")
    })
  })

  observeEvent(input$next_to_network, {
    if (!require_auth("network")) return(invisible(NULL))

    req(rv$annotation_status == "finished")
    switch_page("network")
  })

  # Redundancy summary, told in words first and numbers second: two bar
  # charts carried very little information for the space they took.
  output$annotation_summary_ui <- renderUI({
    if (identical(rv$annotation_status, "running")) {
      return(
        tags$div(
          class = "empty-panel empty-panel-compact",
          icon("spinner", class = "fa-spin"),
          "Annotating features ..."
        )
      )
    }

    if (is.null(rv$annotation_summary)) {
      return(NULL)
    }

    smry <- rv$annotation_summary
    stage <- tolower(as.character(rownames(smry)))

    norm_name <- function(x) gsub("[^a-z0-9]+", "_", tolower(x))
    smry_cols <- norm_name(colnames(smry))

    get_val <- function(col, which_stage) {
      idx <- which(stage == which_stage)
      j <- which(smry_cols == norm_name(col))
      if (length(idx) != 1 || length(j) != 1) return(NA_real_)
      suppressWarnings(as.numeric(smry[idx, j]))
    }

    # Paired columns under the headline number. Tall on purpose: the card
    # has the vertical room and a squat chart looked unfinished.
    metric_chart <- function(before, after, digits) {
      W <- 420
      H <- 300
      base_y <- 250
      top_y <- 22
      plot_h <- base_y - top_y

      vmax <- suppressWarnings(max(c(before, after)[is.finite(c(before, after))]))
      if (!is.finite(vmax) || vmax <= 0) vmax <- 1

      bw <- 104
      x_b <- 76
      x_a <- 240
      h_b <- if (is.finite(before)) plot_h * before / vmax else 0
      h_a <- if (is.finite(after)) plot_h * after / vmax else 0

      col_rect <- function(x, h, fill) {
        r <- 6
        y <- base_y - h
        if (h < r * 2) {
          return(sprintf(
            "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='%s'/>",
            x, y, bw, max(h, 2), fill
          ))
        }
        sprintf(
          paste0(
            "<path d='M %.1f %.1f V %.1f Q %.1f %.1f %.1f %.1f ",
            "H %.1f Q %.1f %.1f %.1f %.1f V %.1f Z' fill='%s'/>"
          ),
          x, base_y, y + r, x, y, x + r, y,
          x + bw - r, x + bw, y, x + bw, y + r, base_y, fill
        )
      }

      HTML(paste0(
        "<svg viewBox='0 0 ", W, " ", H, "' role='img'>",
        # where the bar stood before filtering
        sprintf(
          "<line x1='40' y1='%.1f' x2='380' y2='%.1f' stroke='#dbe4ee' stroke-width='1' stroke-dasharray='3 4'/>",
          base_y - h_b, base_y - h_b
        ),
        col_rect(x_b, h_b, "#cbdaea"),
        col_rect(x_a, h_a, "#2f79af"),
        sprintf("<line x1='40' y1='%.1f' x2='380' y2='%.1f' stroke='#dde5ee' stroke-width='1'/>",
                base_y, base_y),
        sprintf(
          "<text x='%.1f' y='%.1f' text-anchor='middle' font-size='12.5' font-weight='700' letter-spacing='1.1' fill='#a1b1c2'>BEFORE</text>",
          x_b + bw / 2, base_y + 26
        ),
        sprintf(
          "<text x='%.1f' y='%.1f' text-anchor='middle' font-size='12.5' font-weight='700' letter-spacing='1.1' fill='#5c7186'>AFTER</text>",
          x_a + bw / 2, base_y + 26
        ),
        "</svg>"
      ))
    }

    metric_block <- function(col, name, desc, info, digits) {
      before <- get_val(col, "before")
      after <- get_val(col, "after")
      if (!is.finite(before) && !is.finite(after)) return(NULL)

      fmt <- function(x) {
        if (!is.finite(x)) "-" else format(round(x, digits), nsmall = digits, trim = TRUE)
      }

      delta <- if (is.finite(before) && is.finite(after) && before != 0) {
        (after - before) / before * 100
      } else {
        NA_real_
      }

      delta_tag <- if (!is.finite(delta)) {
        NULL
      } else if (delta < -0.05) {
        tags$span(class = "wb-delta is-good", icon("arrow-down"), sprintf("%.1f%%", abs(delta)))
      } else if (delta > 0.05) {
        tags$span(class = "wb-delta is-up", icon("arrow-up"), sprintf("%.1f%%", delta))
      } else {
        tags$span(class = "wb-delta is-flat", "no change")
      }

      tags$div(
        class = "wb-metric",
        tags$div(class = "wb-metric-name", name, build_info_icon(info)),
        tags$div(
          class = "wb-metric-figure",
          tags$span(class = "wb-metric-value", fmt(after)),
          delta_tag
        ),
        tags$div(
          class = "wb-metric-was",
          "was ", tags$b(fmt(before)), " before filtering"
        ),
        tags$div(
          class = "wb-metric-chart",
          metric_chart(before, after, digits)
        ),
        tags$p(class = "wb-metric-desc", desc)
      )
    }

    tags$div(
      tags$div(
        class = "wb-metric-grid",
        metric_block(
          col = "metabolite_per_feature",
          name = "Candidate metabolites per feature",
          desc = paste(
            "On average, how many different metabolites still match one LC-MS feature.",
            "Lower means each feature has fewer competing explanations."
          ),
          info = paste0(
            "Mean number of database compounds matched to a single feature ",
            "(<code>variable_id</code>), averaged over all annotated features."
          ),
          digits = 2
        ),
        metric_block(
          col = "mfc_per_metabolite",
          name = "Feature groups per metabolite",
          desc = paste(
            "On average, how many separate groups of co-eluting features are assigned",
            "to the same metabolite. Lower means its signal is consolidated, not scattered."
          ),
          info = paste0(
            "A <b>metabolite feature cluster (MFC)</b> is a set of features that co-elute ",
            "and share a mass relationship - isotopes, adducts and in-source fragments of ",
            "one compound. This is the mean number of distinct clusters mapped to the same ",
            "compound (<code>Lab.ID</code>); more than one means the same metabolite was ",
            "matched from unrelated places in the chromatogram."
          ),
          digits = 3
        )
      ),
      tags$p(
        class = "wb-metric-note",
        icon("circle-info"),
        " Before and after low-confidence matches were filtered out. Full matches are in the Annotation Table tab."
      )
    )
  })

  pick_first_col <- function(df, candidates) {
    nms <- names(df)
    idx <- match(tolower(candidates), tolower(nms))
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) return(NULL)
    nms[idx[1]]
  }

  output$annotation_empty_state <- renderUI({
    if (identical(rv$annotation_status, "finished")) {
      return(NULL)
    }

    empty_step <- function(num, text, tag = NULL, tag_class = NULL) {
      tags$div(
        class = "wb-empty-step",
        tags$span(class = "wb-empty-step-num", num),
        tags$span(
          class = "wb-empty-step-text",
          HTML(text),
          if (!is.null(tag)) {
            tags$span(class = paste("wb-empty-tag", tag_class), tag)
          }
        )
      )
    }

    if (is.null(rv$feature_data)) {
      return(
        tags$div(
          class = "wb-empty-layer",
          tags$div(
            class = "wb-empty",
            tags$div(class = "wb-empty-icon", icon("circle-exclamation")),
            tags$div(class = "wb-empty-title", "No data loaded yet"),
            tags$div(
              class = "wb-empty-steps",
              empty_step("1", "Go back to <b>Data Upload</b> and load a feature table"),
              empty_step("2", "Then return here to annotate the features")
            )
          )
        )
      )
    }

    tags$div(
      class = "wb-empty-layer",
      tags$div(
        class = "wb-empty",
        tags$div(class = "wb-empty-icon", icon("magnifying-glass")),
        tags$div(class = "wb-empty-title", "Annotate features against the reference"),
        tags$div(
          class = "wb-empty-steps",
          empty_step(
            "1",
            "Set the <b>MS1 ppm</b> and <b>RT tolerance</b> windows used for matching"
          ),
          empty_step(
            "2",
            "Pick the <b>column type</b> so retention time is interpreted correctly"
          ),
          empty_step(
            "3",
            "Isotope peaks are annotated and redundant matches removed automatically"
          ),
          empty_step(
            "4",
            "Click <b>Run</b> to score every feature-metabolite pair and preview it here"
          )
        ),
        tags$p(
          class = "wb-empty-foot",
          "Annotation runs on ", tags$b(format(nrow(rv$feature_data), big.mark = ",")),
          " features. Larger tolerances take longer."
        )
      )
    )
  })

  output$network_empty_state <- renderUI({
    if (identical(rv$pcsf_status, "finished")) return(NULL)

    if (!identical(rv$annotation_status, "finished")) {
      return(wb_empty_state(
        "circle-exclamation",
        "Feature annotation has not run yet",
        list(
          wb_empty_step("1", "Go back to <b>Feature Annotation</b> and run it"),
          wb_empty_step("2", "Then return here to extract the subnetwork")
        )
      ))
    }

    wb_empty_state(
      "diagram-project",
      "Refine the annotations on the metabolic network",
      list(
        wb_empty_step("1", "Choose which features act as <b>terminals</b> - all of them, or only the significant ones"),
        wb_empty_step("2", "Set <b>b</b> and <b>w</b>, or let <b>grid tuning</b> sweep a range and pick the most robust pair"),
        wb_empty_step("3", "PCSF keeps well-supported metabolites and the reactions that connect them"),
        wb_empty_step("4", "Click <b>Run PCSF</b> to build the subnetwork and see it here")
      ),
      foot = "Repeated iterations are averaged, so a node's frequency reflects how reliably it is kept."
    )
  })


  output$modules_empty_state <- renderUI({
    if (identical(rv$module_status, "finished")) return(NULL)

    if (!identical(rv$pcsf_status, "finished")) {
      return(wb_empty_state(
        "circle-exclamation",
        "No subnetwork yet",
        list(
          wb_empty_step("1", "Go back to <b>Subnetwork Extraction</b> and run PCSF"),
          wb_empty_step("2", "Then return here to detect modules")
        )
      ))
    }

    wb_empty_state(
      "layer-group",
      "Split the subnetwork into functional modules",
      list(
        wb_empty_step("1", "Pick a <b>community detection</b> method and its resolution"),
        wb_empty_step("2", "Drop unstable nodes with the <b>frequency threshold</b> and set a minimum module size"),
        wb_empty_step("3", "Each module is scored and tested against a <b>permutation null</b>"),
        wb_empty_step("4", "Click <b>Run</b> to get modules with p-values and FDR")
      ),
      foot = "More permutations give finer p-values but take longer."
    )
  })

  output$function_empty_state <- renderUI({
    if (identical(rv$function_status, "finished")) return(NULL)

    if (!identical(rv$module_status, "finished")) {
      return(wb_empty_state(
        "circle-exclamation",
        "No modules yet",
        list(
          wb_empty_step("1", "Go back to <b>Module Identification</b> and run it"),
          wb_empty_step("2", "Then return here to interpret the modules")
        )
      ))
    }

    wb_empty_state(
      "robot",
      "Interpret each significant module",
      list(
        wb_empty_step("1", "Choose the <b>significance cutoff</b> that decides which modules are interpreted"),
        wb_empty_step("2", "Pick <b>Fast</b> for a single hypothesis, or <b>Advanced</b> for a proposer/skeptic/arbiter debate"),
        wb_empty_step("3", "Add <b>study context</b> and optionally PubMed evidence", "optional", "is-optional"),
        wb_empty_step("4", "Enter your <b>provider, model ID and API key</b>", "required", "is-required"),
        wb_empty_step("5", "Click <b>Run</b> to generate functional summaries and confidence rationales")
      ),
      foot = "Nothing is sent anywhere until you press Run; the API key stays in this session."
    )
  })


  output$annotated_table <- renderDT({
    if (is.null(rv$annotated_table) || nrow(rv$annotated_table) == 0) {
      return(render_table_message("No annotation results yet"))
    }
    wb_preview_table(rv$annotated_table)
  })
  
  ## Step 3: Extract Subnetwork ======================================
  observeEvent(input$run_pcsf, {
    if (!require_auth("network")) return(invisible(NULL))
    
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
        show_error_modal("Invalid grid range", "b min must be less than or equal to b max.")
        shinyjs::enable("run_pcsf")
        rv$pcsf_status <- "idle"
        return()
      }
      if (input$pcsf_w_min > input$pcsf_w_max) {
        show_error_modal("Invalid grid range", "w min must be less than or equal to w max.")
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
        update_process_modal("error", "Step 3 Failed", res$message)
        close_process_modal(1800)
      } else {
        rv$annotation_object <- res
        rv$annotation_table2 <- res$anno_feature_table2
        rv$pcsf_status <- "finished"
        rv$pcsf_selected_node <- NULL

        g <- res$pcsf_subnet
        fmt_n <- function(x) if (is.null(x) || is.na(x)) "-" else format(x, big.mark = ",")
        finish_process_modal(
          "Subnetwork extracted",
          "You can continue to Module Identification.",
          list(
            proc_row("circle-nodes", "Metabolite nodes",
                     fmt_n(if (methods::is(g, "igraph")) igraph::vcount(g) else NA_integer_)),
            proc_row("link", "Reaction edges",
                     fmt_n(if (methods::is(g, "igraph")) igraph::gsize(g) else NA_integer_)),
            proc_row("table", "Annotation table 2",
                     fmt_n(if (is.data.frame(res$anno_feature_table2)) nrow(res$anno_feature_table2) else NA_integer_))
          )
        )
      }
      
      shinyjs::enable("run_pcsf")
    })
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
        # "100%" (not a pixel value) is what lets the canvas fill the flex
        # stage: visNetworkOutput writes the height as an INLINE style, which
        # outranks the .pcsf-network-stage rule below and would otherwise pin
        # the canvas to a fixed height inside a taller box.
        visNetworkOutput("pcsf_network_plot", height = "100%", width = "100%"),
        uiOutput("pcsf_filter_panel")
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
      ),
      tags$span()
    )
  })

  # Reproducibility filters, floated over the canvas so the topbar keeps
  # only the two node selectors.
  output$pcsf_filter_panel <- renderUI({
    tags$div(
      class = "pcsf-filter-panel",
      tags$div(
        class = "pcsf-filter-head",
        icon("filter"),
        "Reproducibility",
        build_info_icon(
          paste0(
            "PCSF is repeated many times; <b>frequency</b> is the share of ",
            "repeats in which a node - or an edge - appeared, so it measures ",
            "how reproducible that part of the subnetwork is.<br><br>",
            "<b>Node</b> and <b>edge</b> thresholds are independent. A node ",
            "can be perfectly stable while every connection to it is not, so ",
            "any node left without an edge is dropped from the view.<br><br>",
            "<code>0</code> on both shows everything."
          )
        )
      ),
      tags$div(
        class = "pcsf-filter-row",
        tags$span(class = "pcsf-filter-label", "Node freq"),
        numericInput("pcsf_node_freq_cutoff", label = NULL,
                     value = 0, min = 0, max = 1, step = 0.05)
      ),
      tags$div(
        class = "pcsf-filter-row",
        tags$span(class = "pcsf-filter-label", "Edge freq"),
        numericInput("pcsf_edge_freq_cutoff", label = NULL,
                     value = 0, min = 0, max = 1, step = 0.05)
      ),
      uiOutput("pcsf_visible_count")
    )
  })

  output$pcsf_visible_count <- renderUI({
    if (!(rv$pcsf_status == "finished" && isTRUE(input$pcsf_show_network))) {
      return(NULL)
    }
    d <- pcsf_network_base_data()
    if (is.null(d$total_nodes)) return(NULL)

    tags$div(
      class = "pcsf-filter-count",
      tags$div(
        tags$b(format(d$shown_nodes, big.mark = ",")),
        " / ",
        format(d$total_nodes, big.mark = ","),
        " nodes"
      ),
      tags$div(
        tags$b(format(d$shown_edges, big.mark = ",")),
        " / ",
        format(d$total_edges, big.mark = ","),
        " edges"
      ),
      if (isTRUE(d$dropped_isolated > 0)) {
        tags$div(
          class = "pcsf-filter-note",
          format(d$dropped_isolated, big.mark = ","), " unconnected hidden"
        )
      }
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

    # The PCSF subnet only carries name/prize/type - compound names live on
    # the MRN vertices (and, failing that, in annotation table 2).
    node_compound_name <- rep(NA_character_, length(node_id))

    if (!is.null(rv$mrn) && methods::is(rv$mrn, "igraph")) {
      mrn_nodes <- tryCatch(
        as.data.frame(igraph::as_data_frame(rv$mrn, what = "vertices"),
                      stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(mrn_nodes) && nrow(mrn_nodes) > 0) {
        key_col <- pick_first_col(mrn_nodes, c("name", "INCHI_ID", "inchi_id"))
        name_col <- pick_first_col(
          mrn_nodes,
          c("Compound_name", "compound_name", "Compound.name", "name_compound")
        )
        if (!is.null(key_col) && !is.null(name_col)) {
          idx <- match(node_id, as.character(mrn_nodes[[key_col]]))
          node_compound_name <- as.character(mrn_nodes[[name_col]])[idx]
        }
      }
    }

    missing_name <- is.na(node_compound_name) | !nzchar(node_compound_name)
    if (any(missing_name) && !is.null(rv$annotation_table2)) {
      tbl2 <- rv$annotation_table2
      key2 <- pick_first_col(tbl2, c("INCHI_ID", "inchi_id"))
      name2 <- pick_first_col(tbl2, c("Compound.name", "Compound_name", "compound_name"))
      if (!is.null(key2) && !is.null(name2)) {
        map2 <- tbl2[!duplicated(tbl2[[key2]]), c(key2, name2), drop = FALSE]
        idx2 <- match(node_id[missing_name], as.character(map2[[key2]]))
        node_compound_name[missing_name] <- as.character(map2[[name2]])[idx2]
      }
    }

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
      compound_name = node_compound_name,
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

  # Debounced so dragging the threshold does not rebuild the widget on
  # every tick.
  clamp_freq <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) != 1 || !is.finite(v)) 0 else max(0, min(1, v))
  }

  pcsf_node_freq_cutoff <- reactive({
    clamp_freq(input$pcsf_node_freq_cutoff)
  }) %>% shiny::debounce(400)

  pcsf_edge_freq_cutoff <- reactive({
    clamp_freq(input$pcsf_edge_freq_cutoff)
  }) %>% shiny::debounce(400)

  # The layout is expensive and must not move when the threshold changes,
  # so it is computed once per subnetwork and reused.
  pcsf_layout <- reactive({
    req(rv$annotation_object$pcsf_subnet)
    g <- rv$annotation_object$pcsf_subnet
    node_id <- as.character(igraph::V(g)$name)

    coords <- tryCatch({
      set.seed(123)
      if (igraph::vcount(g) > 2500) {
        igraph::layout_with_drl(g)
      } else {
        igraph::layout_with_fr(g, niter = 900)
      }
    }, error = function(e) {
      tryCatch(igraph::layout_nicely(g), error = function(e2) NULL)
    })

    if (is.null(coords) || nrow(coords) != length(node_id)) {
      coords <- matrix(0, nrow = length(node_id), ncol = 2)
    }

    spread <- 46 * sqrt(max(length(node_id), 1))
    norm_axis <- function(v) {
      rng <- range(v, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0, length(v)))
      (v - mean(rng)) / (diff(rng) / 2) * spread
    }

    data.frame(
      id = node_id,
      x = norm_axis(coords[, 1]),
      y = norm_axis(coords[, 2]),
      stringsAsFactors = FALSE
    )
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

    has_name <- !is.na(lookup$compound_name) & nzchar(lookup$compound_name)

    esc <- function(x) htmltools::htmlEscape(as.character(x))

    # vis.js owns .vis-tooltip and forces white-space:nowrap on it, so the
    # width has to be pinned on our own wrapper inside the tooltip.
    # Features have no compound name by definition, so the name line is
    # only emitted when there actually is one.
    name_line <- ifelse(
      has_name,
      paste0(
        "<div style='font-size:13px;font-weight:700;color:#1c3450;",
        "line-height:1.35;margin-bottom:5px;'>",
        esc(lookup$compound_name), "</div>"
      ),
      ""
    )

    tooltip <- paste0(
      "<div style='width:300px;white-space:normal;word-break:break-word;",
      "overflow-wrap:anywhere;font-family:inherit;'>",
      "<div style='font-size:10px;font-weight:700;letter-spacing:0.08em;",
      "text-transform:uppercase;color:#8fa3b8;margin-bottom:4px;'>",
      esc(lookup$group), "</div>",
      name_line,
      "<div style='font-family:ui-monospace,Menlo,Consolas,monospace;",
      "font-size:10.5px;color:#6d8299;line-height:1.45;'>",
      esc(node_id), "</div>",
      "</div>"
    )

    # Coordinates come from the cached layout so vis.js can run with physics
    # switched off - the force simulation over ~10^3 nodes was what made
    # dragging and zooming stutter.
    lay <- pcsf_layout()
    lay_idx <- match(node_id, lay$id)

    nodes <- data.frame(
      id = node_id,
      label = "",
      title = tooltip,
      x = lay$x[lay_idx],
      y = lay$y[lay_idx],
      # area, not radius, tracks frequency, so the difference is readable
      size = {
        pr <- node_prize
        rng <- range(pr[is.finite(pr)], na.rm = TRUE)
        u <- if (!all(is.finite(rng)) || diff(rng) == 0) {
          rep(0.5, length(pr))
        } else {
          (pr - rng[1]) / diff(rng)
        }
        u[!is.finite(u)] <- 0.5
        round(6 + 22 * sqrt(u), 2)
      },
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
      value = scales::rescale(edge_freq, to = c(0.3, 1.8)),
      title = paste0("freq: ", round(edge_freq, 3)),
      color = "rgba(139,152,168,0.26)",
      stringsAsFactors = FALSE
    )

    total_nodes <- nrow(nodes)
    total_edges <- nrow(edges)

    # Two independent reproducibility levels. A node can be perfectly stable
    # while every edge touching it is not, so anything left without an edge
    # is dropped rather than drawn as a stray dot.
    node_cut <- pcsf_node_freq_cutoff()
    edge_cut <- pcsf_edge_freq_cutoff()
    dropped_isolated <- 0L

    if (node_cut > 0 || edge_cut > 0) {
      keep_node <- !is.finite(node_prize) | node_prize >= node_cut
      keep_edge <- !is.finite(edge_freq) | edge_freq >= edge_cut

      if (any(keep_node)) {
        nodes <- nodes[keep_node, , drop = FALSE]
        edges <- edges[keep_edge, , drop = FALSE]
        edges <- edges[edges$from %in% nodes$id & edges$to %in% nodes$id, , drop = FALSE]

        connected <- unique(c(edges$from, edges$to))
        dropped_isolated <- sum(!(nodes$id %in% connected))
        nodes <- nodes[nodes$id %in% connected, , drop = FALSE]
      }
    }

    list(
      nodes = nodes,
      edges = edges,
      total_nodes = total_nodes,
      total_edges = total_edges,
      shown_nodes = nrow(nodes),
      shown_edges = nrow(edges),
      dropped_isolated = dropped_isolated
    )
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

    visNetwork(graph_data$nodes, graph_data$edges, width = "100%", height = "100%") %>%
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
          min = 6,
          max = 28,
          label = list(
            enabled = FALSE,
            min = 14,
            max = 14
          )
        )
      ) %>%
      visEdges(
        smooth = FALSE,
        width = 1
      ) %>%
      visInteraction(
        hoverConnectedEdges = FALSE,
        selectConnectedEdges = FALSE,
        hideEdgesOnDrag = TRUE,
        hideEdgesOnZoom = TRUE,
        tooltipDelay = 120
      ) %>%
      visEvents(
        click = "function(params) {
          var selected = params.nodes && params.nodes.length ? params.nodes[0] : null;
          Shiny.setInputValue('pcsf_network_selected_node', selected, {priority: 'event'});
        }"
      ) %>%
      # positions come from the pre-computed layout, so no force simulation
      visPhysics(enabled = FALSE, stabilization = FALSE) %>%
      visOptions(autoResize = TRUE)
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

    rv$pcsf_selector_syncing <- TRUE
    rv$pcsf_selector_sync_token <- rv$pcsf_selector_sync_token + 1L
    sync_token <- rv$pcsf_selector_sync_token
    shinyjs::delay(500, {
      if (identical(rv$pcsf_selector_sync_token, sync_token)) {
        rv$pcsf_selector_syncing <- FALSE
      }
    })

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
    if (isTRUE(rv$pcsf_selector_syncing)) {
      return()
    }

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
    if (isTRUE(rv$pcsf_selector_syncing)) {
      return()
    }

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
    
    wb_preview_table(rv$annotation_table2)
  })

  observeEvent(input$next_to_modules, {
    if (!require_auth("modules")) return(invisible(NULL))

    req(rv$pcsf_status == "finished")
    switch_page("modules")
  })
  
 
  ## Step 4: Module Identification =====================================
  observeEvent(input$run_modules, {
    if (!require_auth("modules")) return(invisible(NULL))

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
        update_process_modal("error", "Step 4 Failed", res$message)
        close_process_modal(1800)
      } else {
        rv$module_res    <- res
        rv$mrn           <- res$mrn
        rv$module_status <- "finished"

        score_tbl <- res$module_score
        n_modules <- 0L
        n_p <- NA_integer_
        n_fdr <- NA_integer_
        if (is.data.frame(score_tbl) && nrow(score_tbl) > 0) {
          module_col <- pick_first_col(score_tbl, c("Module", "module", "module_id"))
          n_modules <- if (!is.null(module_col)) {
            length(unique(stats::na.omit(score_tbl[[module_col]])))
          } else nrow(score_tbl)
          count_sig <- function(cands) {
            col <- pick_first_col(score_tbl, cands)
            if (is.null(col)) return(NA_integer_)
            v <- suppressWarnings(as.numeric(score_tbl[[col]]))
            sum(is.finite(v) & v < 0.05, na.rm = TRUE)
          }
          n_p <- count_sig(c("p", "pvalue", "p_value", "P", "P.Value"))
          n_fdr <- count_sig(c("fdr", "FDR", "qvalue", "q_value", "padj", "p_adj"))
        }

        finish_process_modal(
          "Modules identified",
          "You can continue to Function Interpretation.",
          list(
            proc_row("layer-group", "Modules detected", format(n_modules, big.mark = ",")),
            proc_row("star", "p < 0.05", if (is.na(n_p)) "-" else format(n_p, big.mark = ",")),
            proc_row("filter-circle-dollar", "FDR < 0.05", if (is.na(n_fdr)) "-" else format(n_fdr, big.mark = ","))
          )
        )
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
  
  
  observeEvent(list(rv$module_status, rv$module_res$module_score), ignoreInit = FALSE, {
    if (rv$module_status != "finished" || is.null(rv$module_res$module_score)) {
      updateSelectizeInput(
        session,
        "selected_module",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    mods <- unique(as.character(rv$module_res$module_score$Module))
    mod_num <- suppressWarnings(as.numeric(gsub("[^0-9]+", "", mods)))
    mods <- mods[order(is.na(mod_num), mod_num, mods)]

    current_selected <- isolate(input$selected_module)
    selected_mod <- if (!is.null(current_selected) && current_selected %in% mods) {
      current_selected
    } else if (length(mods) > 0) {
      mods[1]
    } else {
      character(0)
    }

    updateSelectizeInput(
      session,
      "selected_module",
      choices = mods,
      selected = selected_mod
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

    # Fixed coordinates so vis.js can run with physics off: with the force
    # simulation live the nodes never settled and kept drifting.
    coords <- tryCatch({
      set.seed(123)
      igraph::layout_with_fr(g, niter = 900)
    }, error = function(e) {
      tryCatch(igraph::layout_nicely(g), error = function(e2) NULL)
    })

    if (is.null(coords) || nrow(coords) != length(node_id)) {
      coords <- matrix(0, nrow = length(node_id), ncol = 2)
    }

    spread <- 62 * sqrt(max(length(node_id), 1))
    norm_axis <- function(v) {
      rng <- range(v, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0, length(v)))
      (v - mean(rng)) / (diff(rng) / 2) * spread
    }

    nodes <- data.frame(
      id = node_id,
      label = ifelse(
        nchar(node_label_full) > 28,
        paste0(substr(node_label_full, 1, 25), "..."),
        node_label_full
      ),
      title = node_label_full,
      # vis.js defaults to 25, which dwarfed the 14px labels
      size = 13,
      x = norm_axis(coords[, 1]),
      y = norm_axis(coords[, 2]),
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

    visNetwork(graph_data$nodes, graph_data$edges, width = "100%", height = "100%") %>%
      visNodes(
        borderWidth = 0,
        borderWidthSelected = 0,
        font = list(size = 14, face = "Arial", color = "#475467"),
        scaling = list(
          label = list(enabled = FALSE)
        )
      ) %>%
      visEdges(smooth = FALSE) %>%
      visInteraction(
        hoverConnectedEdges = FALSE,
        selectConnectedEdges = FALSE,
        hideEdgesOnDrag = TRUE,
        hideEdgesOnZoom = TRUE,
        tooltipDelay = 120
      ) %>%
      visEvents(
        click = "function(params) {
          var selected = params.nodes && params.nodes.length ? params.nodes[0] : null;
          Shiny.setInputValue('module_network_selected_node', selected, {priority: 'event'});
        }"
      ) %>%
      # coordinates are pre-computed, so no force simulation has to converge
      visPhysics(enabled = FALSE, stabilization = FALSE)
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
    
    wb_preview_table(rv$module_res$module_score)
  })
  
  output$module_table <- renderDT({
    if (is.null(rv$module_res) ||
        is.null(rv$module_res$module_table) ||
        nrow(rv$module_res$module_table) == 0) {
      return(render_table_message("No module membership results yet"))
    }
    
    wb_preview_table(rv$module_res$module_table)
  })


  
  observeEvent(input$next_to_function, {
    if (!require_auth("function")) return(invisible(NULL))

    req(rv$module_status == "finished")
    switch_page("function")
  })
  
  
  ## Step 5: Function Interpretation =====================================
  observeEvent(input$run_function_annotation, {
    if (!require_auth("function")) return(invisible(NULL))
    
    req(rv$module_res)
    req(rv$module_status == "finished")
    
    # ---- basic checks ----
    req(nzchar(input$func_llm_model))
    
    llm_key <- trimws(input$func_llm_key)
    if (!nzchar(llm_key)) {
      llm_key <- trimws(Sys.getenv("FEATUREMAP_LLM_API_KEY", unset = ""))
    }
    if (!identical(input$func_provider, "local") && !nzchar(llm_key)) {
      show_error_modal(
        "LLM API key missing",
        "Fill the API key input, or set FEATUREMAP_LLM_API_KEY in the environment."
      )
      return()
    }

    llm_base_url <- NULL
    if (identical(input$func_provider, "local")) {
      llm_base_url <- trimws(input$func_base_url)
      if (!nzchar(llm_base_url)) {
        llm_base_url <- trimws(Sys.getenv("FEATUREMAP_LLM_BASE_URL", unset = ""))
      }
      if (!nzchar(llm_base_url)) {
        show_error_modal(
          "Local base URL missing",
          "Fill the base URL input, or set FEATUREMAP_LLM_BASE_URL in the environment."
        )
        return()
      }
    }
    
    if (isTRUE(input$func_use_pubmed)) {
      ncbi_key <- trimws(input$func_ncbi_key)
      if (!nzchar(ncbi_key)) {
        ncbi_key <- trimws(Sys.getenv("FEATUREMAP_NCBI_API_KEY", unset = ""))
      }
      if (!nzchar(ncbi_key)) {
        show_error_modal(
          "NCBI API key missing",
          "Fill the NCBI key input, or set FEATUREMAP_NCBI_API_KEY in the environment."
        )
        return()
      }
    } else {
      ncbi_key <- trimws(input$func_ncbi_key)
      if (!nzchar(ncbi_key)) {
        ncbi_key <- trimws(Sys.getenv("FEATUREMAP_NCBI_API_KEY", unset = ""))
      }
    }
    
    # Reference database is always the built-in FeatureMAP database.
    ref_data <- NULL

    func_mode <- if (identical(input$func_mode, "advanced")) "advanced" else "fast"
    # Cross-module synthesis only runs in Advanced mode, and only if the
    # user checked the box for it.
    # Cross-module synthesis is part of Advanced mode, not a separate opt-in.
    func_synthesize  <- func_mode == "advanced"
    # The per-module debate transcript HTML always generates in Advanced
    # mode (needed to render the Debate History tab and to make it
    # available on the Results Download page).
    func_debate_html <- func_mode == "advanced"

    # Always give this run its own directory, in both fast and advanced
    # mode. Writing generated HTML into getwd() would make concurrent
    # sessions overwrite each other's synthesis_network.html.
    function_html_dir <- session_html_dir(session)
    rv$function_html_dir <- function_html_dir
    rv$function_debate_html_dir <- function_html_dir

    show_process_modal(
      title = "Step 5 Running",
      text = "Module interpretation is in progress. Please wait."
    )
    
    # ---- set status ----
    rv$function_status <- "running"
    rv$function_pubmed_used <- isTRUE(input$func_use_pubmed)
    rv$function_mode_used <- func_mode
    rv$function_debate_html_used <- func_debate_html
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
          mode          = func_mode,
          pubmed        = input$func_use_pubmed,
          years         = input$func_pubmed_years,
          provider      = input$func_provider,
          llm_model     = input$func_llm_model,
          NCBI_api_key  = ncbi_key,
          llm_api_key   = llm_key,
          base_url      = llm_base_url,
          synthesize      = func_synthesize,
          synthesis_model = input$func_llm_model,
          debate_html     = func_debate_html,
          html_dir        = function_html_dir
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
        
        update_process_modal(
          "error",
          "Step 5 Failed",
          paste("Functional interpretation failed:", res$message)
        )
        close_process_modal(2200)
        
      } else {
        # ---- normalize empty result as "finished" ----
        mf <- res$module_function
        is_empty <- is.null(mf) || !is.data.frame(mf) || nrow(mf) == 0

        rv$function_result <- res
        rv$function_status <- "finished"

        rv$function_synthesis_html_path <- NULL
        if (!is.null(res$module_synthesis)) {
          candidate <- file.path(function_html_dir, "synthesis_network.html")
          if (!file.exists(candidate)) {
            candidate <- tryCatch(
              export_synthesis_network_html(
                res = res,
                html_dir = function_html_dir,
                output_file = "synthesis_network.html",
                show_same_module = FALSE
              ),
              error = function(e) NULL
            )
          }
          if (file.exists(candidate)) {
            rv$function_synthesis_html_path <- candidate
          }
        }

        # (function_module_selector rebuilds itself via renderUI whenever
        # rv$function_result/rv$function_status change, so no manual
        # update*Input() call is needed here.)

        mf <- rv$function_result$module_function
        n_mod <- if (is.data.frame(mf) && "module_id" %in% names(mf)) {
          length(unique(stats::na.omit(mf$module_id)))
        } else 0L

        finish_process_modal(
          "Interpretation finished",
          "You can continue to Results Download.",
          list(
            proc_row("layer-group", "Modules interpreted", format(n_mod, big.mark = ",")),
            proc_row("wand-magic-sparkles", "Mode",
                     if (identical(rv$function_mode_used, "advanced")) "Advanced" else "Fast")
          )
        )

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
        "Generating AI-powered functional interpretation..."
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

      if (identical(rv$function_mode_used, "advanced")) {
        tags$div(
          class = "function-result-shell",
          tabsetPanel(
            id = "function_result_tabs",
            type = "tabs",
            tabPanel(
              title = tagList(icon("circle-check"), "Accepted Interpretations"),
              tags$div(
                class = "fast-toolbar",
                tags$span(
                  class = "fast-function-eyebrow",
                  icon("users"), "Advanced mode - accepted hypothesis"
                ),
                tags$div(
                  class = "fast-toolbar-right",
                  uiOutput("function_module_selector"),
                  uiOutput("function_hypothesis_selector")
                )
              ),
              uiOutput("function_module_output")
            ),
            tabPanel(
              title = tagList(icon("comments"), "Debate History"),
              uiOutput("function_debate_ui")
            ),
            tabPanel(
              title = tagList(icon("diagram-project"), "Cross-Module Synthesis"),
              uiOutput("function_synthesis_ui")
            )
          )
        )
      } else {
        tags$div(
          class = "function-result-shell",
          tags$div(
            class = "fast-toolbar",
            tags$span(
              class = "fast-function-eyebrow",
              icon("bolt"), "Fast mode interpretation"
            ),
            tags$div(
              class = "fast-toolbar-right",
              uiOutput("function_module_selector")
            )
          ),
          uiOutput("function_module_output")
        )
      }

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
      id = "function_module_selector_ui",
      class = "fast-select",
      tags$span(class = "fast-select-label", "Module"),
      tags$div(
        class = "fast-select-field",
        selectizeInput(
          "selected_function_module",
          label = NULL,
          choices = module_ids,
          selected = module_ids[1],
          multiple = FALSE,
          width = "100%",
          options = list(placeholder = "Module")
        )
      )
    )
  })

  #### ---- Hypothesis selector (advanced mode only) ----
  output$function_hypothesis_selector <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (!identical(rv$function_mode_used, "advanced")) return(NULL)
    if (is.null(rv$function_result) || is.null(rv$function_result$module_function)) return(NULL)
    if (is.null(input$selected_function_module)) return(NULL)

    df <- rv$function_result$module_function
    if (!is.data.frame(df) ||
        !all(c("module_id", "hypothesis_id", "verdict_status") %in% names(df))) {
      return(NULL)
    }

    sub <- df[as.character(df$module_id) == as.character(input$selected_function_module), , drop = FALSE]
    accepted <- sub[!is.na(sub$verdict_status) & sub$verdict_status == "accepted", , drop = FALSE]

    if (nrow(accepted) == 0) {
      return(
        tags$span(class = "fast-muted", "No accepted hypothesis for this module.")
      )
    }

    choices <- unique(as.character(accepted$hypothesis_id))

    tags$div(
      id = "function_hypothesis_selector_ui",
      class = "fast-select",
      tags$span(class = "fast-select-label", "Hypothesis"),
      tags$div(
        class = "fast-select-field is-wide",
        selectizeInput(
          "selected_function_hypothesis",
          label = NULL,
          choices = choices,
          selected = choices[1],
          multiple = FALSE,
          width = "100%",
          options = list(placeholder = "Hypothesis")
        )
      )
    )
  })


  #### ---- Module annotation output ----
  # (Literature evidence is now rendered inline, per accepted hypothesis,
  # inside render_fast_module()'s "Phenotype & Literature" section — no
  # separate standalone literature panel is needed any more.)
  output$function_module_output <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (is.null(rv$function_result) || is.null(rv$function_result$module_function)) return(NULL)
    if (is.null(input$selected_function_module)) return(NULL)

    mod_df <- rv$function_result$module_function %>%
      dplyr::filter(as.character(module_id) == as.character(input$selected_function_module))

    req(nrow(mod_df) >= 1)

    get_field <- function(row, field, fallback = NULL) {
      if (field %in% names(row) && !is.na(row[[field]][1]) && nzchar(trimws(as.character(row[[field]][1])))) {
        as.character(row[[field]][1])
      } else {
        fallback
      }
    }

    split_semicolon <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x[1])) return(character(0))
      parts <- trimws(strsplit(as.character(x[1]), ";\\s*")[[1]])
      parts <- parts[nzchar(parts) & !(toupper(parts) %in% c("NA", "NULL", "UNKNOWN"))]
      unique(parts)
    }

    compact_text <- function(x) {
      if (is.null(x) || length(x) == 0) return(character(0))
      x <- trimws(as.character(unlist(x, use.names = FALSE)))
      x[!is.na(x) & nzchar(x) & !(toupper(x) %in% c("NA", "NULL", "UNKNOWN"))]
    }

    as_finite_number <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_real_)
      val <- suppressWarnings(as.numeric(x[1]))
      if (is.finite(val)) val else NA_real_
    }

    fmt_pct <- function(x) {
      val <- as_finite_number(x)
      if (is.na(val)) return(NULL)
      if (abs(val) <= 1) val <- val * 100
      paste0(round(val, 1), "%")
    }

    fmt_cell <- function(x, percent = FALSE) {
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      if (length(x) == 0 || all(is.na(x))) return("–")
      if (percent) {
        pct <- fmt_pct(x[1])
        if (!is.null(pct)) return(pct)
      }
      x <- compact_text(x)
      if (length(x) == 0) "–" else paste(x, collapse = ", ")
    }

    conf_class <- function(x) {
      lab <- tolower(trimws(as.character(x %||% "")))
      if (lab %in% c("high", "medium", "low")) paste0("conf-", lab) else ""
    }

    direction_chip <- function(x) {
      lab <- tolower(trimws(as.character(x %||% "")))
      icon_name <- switch(lab,
        up = "arrow-trend-up",
        down = "arrow-trend-down",
        "arrows-left-right"
      )
      dir_cls <- if (lab %in% c("up", "down")) paste0("dir-", lab) else "dir-mixed"
      label <- if (nzchar(lab)) paste0(toupper(substr(lab, 1, 1)), substr(lab, 2, nchar(lab))) else "–"
      tags$span(class = paste("fast-chip", dir_cls), icon(icon_name), label)
    }

    chip_list <- function(items, empty = "Not available.", class = "fast-chip") {
      items <- compact_text(items)
      if (length(items) == 0) {
        return(tags$span(class = "fast-muted", empty))
      }
      tags$div(
        class = "fast-chip-list",
        lapply(items, function(item) tags$span(class = class, item))
      )
    }

    fast_field <- function(label, value) {
      tags$div(
        class = "fast-field",
        tags$div(class = "fast-field-label", label),
        tags$div(class = "fast-field-value", value)
      )
    }

    fast_section <- function(icon_name, title, accent = "blue", ...) {
      body <- list(...)
      tags$section(
        class = paste0("fast-info-section accent-", accent),
        tags$div(
          class = "fast-section-head",
          tags$span(class = "fast-section-icon", icon(icon_name)),
          tags$h4(title)
        ),
        do.call(tags$div, c(list(class = "fast-section-body"), body))
      )
    }

    # The summary is the headline claim, so it gets a tinted callout rather
    # than being one more label/value row.
    fast_callout <- function(text) {
      tags$div(class = "fast-callout", tags$p(text))
    }

    get_evidence_df <- function(path) {
      ev <- rv$function_result$module_evidence
      if (is.null(ev)) return(NULL)
      df <- ev
      for (p in path) {
        if (is.null(df[[p]])) return(NULL)
        df <- df[[p]]
      }
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      if ("module_id" %in% names(df)) {
        df <- df[as.character(df$module_id) == as.character(input$selected_function_module), , drop = FALSE]
      }
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      df
    }

    mini_table <- function(df, cols, labels, percent_cols = character(0), max_rows = 6, empty = "Not available.") {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(tags$p(class = "fast-muted", empty))
      }
      cols <- cols[cols %in% names(df)]
      if (length(cols) == 0) return(tags$p(class = "fast-muted", empty))
      df <- utils::head(df, max_rows)
      tags$table(
        class = "fast-mini-table",
        tags$thead(
          tags$tr(lapply(cols, function(col) tags$th(labels[[col]] %||% col)))
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(lapply(cols, function(col) {
              tags$td(fmt_cell(df[[col]][i], percent = col %in% percent_cols))
            }))
          })
        )
      )
    }

    # Show ONE representation per field: the structured table when it is
    # available (it carries more detail), otherwise fall back to a compact
    # chip list built from the summary text. Never show both, to avoid
    # repeating the same information twice.
    evidence_content <- function(chip_items, table_df, table_cols, table_labels,
                                  percent_cols = character(0), max_rows = 6,
                                  chip_class = "fast-chip", empty = "Not available.") {
      if (!is.null(table_df) && is.data.frame(table_df) && nrow(table_df) > 0) {
        mini_table(table_df, cols = table_cols, labels = table_labels,
                   percent_cols = percent_cols, max_rows = max_rows, empty = empty)
      } else {
        chip_list(chip_items, empty, chip_class)
      }
    }

    evidence_field <- function(label, chip_items, table_df, table_cols, table_labels,
                                percent_cols = character(0), max_rows = 6, empty = "Not available.") {
      fast_field(
        label,
        evidence_content(chip_items, table_df, table_cols, table_labels, percent_cols, max_rows, empty = empty)
      )
    }

    # Clickable PMID pills -- the Literature support display in both
    # Fast and Advanced mode.
    pmid_link_list <- function(pmids, empty = "No retained PubMed references.") {
      pmids <- compact_text(pmids)
      if (length(pmids) == 0) {
        return(tags$p(class = "fast-muted", empty))
      }
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
    }

    # KEGG / HMDB pathway coverage as grouped ID + description cards
    # (mirrors the compact reference-card layout, grouped by database).
    pathway_card_group <- function(label, df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
          !all(c("pathway_id", "pathway_name") %in% names(df))) {
        return(NULL)
      }
      tags$div(
        class = "fast-pathway-group",
        tags$div(class = "fast-pathway-group-title", label),
        tags$div(
          class = "fast-pathway-grid",
          lapply(seq_len(nrow(df)), function(i) {
            pid  <- as.character(df$pathway_id[i])
            pname <- as.character(df$pathway_name[i])
            hit  <- if ("n_hit" %in% names(df)) suppressWarnings(as.numeric(df$n_hit[i])) else NA
            tot  <- if ("n_metabolites" %in% names(df)) suppressWarnings(as.numeric(df$n_metabolites[i])) else NA
            pct  <- if ("coverage" %in% names(df)) fmt_pct(df$coverage[i]) else NULL
            stats_parts <- compact_text(c(
              if (!is.na(hit) && !is.na(tot)) paste0(hit, "/", tot),
              pct
            ))
            detail <- if (length(stats_parts) > 0) {
              paste0(pname, " (", paste(stats_parts, collapse = ", "), ")")
            } else {
              pname
            }
            tags$div(
              class = "fast-pathway-card",
              tags$div(class = "fast-pathway-id", pid),
              tags$div(class = "fast-pathway-desc", detail)
            )
          })
        )
      )
    }

    # Segmented origin bar (endogenous / exogenous / unknown) with a
    # colored-dot legend underneath, instead of plain text chips.
    origin_bar <- function(endo_num, exo_num, unknown_num) {
      if (is.na(endo_num) || is.na(exo_num)) return(NULL)
      if (is.na(unknown_num)) unknown_num <- 0
      total <- endo_num + exo_num + unknown_num
      if (total <= 0) return(NULL)

      endo_pct <- round(100 * endo_num / total, 1)
      exo_pct  <- round(100 * exo_num / total, 1)
      unk_pct  <- round(100 * unknown_num / total, 1)

      tags$div(
        class = "fast-origin-wrap",
        tags$div(
          class = "fast-origin-bar",
          if (endo_pct > 0) tags$div(class = "fast-origin-seg fast-origin-seg-endo", style = paste0("width:", endo_pct, "%;")),
          if (exo_pct > 0) tags$div(class = "fast-origin-seg fast-origin-seg-exo", style = paste0("width:", exo_pct, "%;")),
          if (unk_pct > 0) tags$div(class = "fast-origin-seg fast-origin-seg-unknown", style = paste0("width:", unk_pct, "%;"))
        ),
        tags$div(
          class = "fast-origin-legend",
          tags$span(class = "fast-origin-legend-item", tags$span(class = "fast-origin-dot fast-origin-dot-endo"), paste0("Endogenous ", endo_pct, "%")),
          tags$span(class = "fast-origin-legend-item", tags$span(class = "fast-origin-dot fast-origin-dot-exo"), paste0("Exogenous ", exo_pct, "%")),
          tags$span(class = "fast-origin-legend-item", tags$span(class = "fast-origin-dot fast-origin-dot-unknown"), paste0("Unknown ", unk_pct, "%"))
        )
      )
    }

    render_fast_module <- function(row) {
      title <- get_field(row, "title", "Untitled module function")
      confidence <- get_field(row, "confidence_label", "–")
      hypothesis_type <- get_field(row, "hypothesis_type", "–")
      direction <- get_field(row, "global_direction", "–")
      module_id <- get_field(row, "module_id", as.character(input$selected_function_module))
      hyp_id <- get_field(row, "hypothesis_id")

      top_df <- get_evidence_df(c("top_metabolites"))
      anchor_df <- get_evidence_df(c("anchor_metabolites"))

      kegg_df <- get_evidence_df(c("pathway_coverage", "kegg"))
      hmdb_df <- get_evidence_df(c("pathway_coverage", "hmdb"))

      endo_num <- as_finite_number(row$origin_endogenous_frac)
      exo_num <- as_finite_number(row$origin_exogenous_frac)
      unknown_num <- if (!is.na(endo_num) && !is.na(exo_num)) {
        max(0, 1 - endo_num - exo_num)
      } else {
        NA_real_
      }

      anchor_items <- split_semicolon(get_field(row, "anchor_metabolites", ""))
      if (length(anchor_items) == 0 && !is.null(anchor_df) && "anchor_name" %in% names(anchor_df)) {
        anchor_items <- compact_text(anchor_df$anchor_name)
      }
      top_items <- split_semicolon(get_field(row, "top_metabolites", ""))
      if (length(top_items) == 0 && !is.null(top_df) && "Compound_name" %in% names(top_df)) {
        ordered_top_df <- top_df
        if ("top_rank" %in% names(ordered_top_df)) {
          ordered_top_df <- ordered_top_df[order(suppressWarnings(as.numeric(ordered_top_df$top_rank))), , drop = FALSE]
        }
        top_items <- compact_text(ordered_top_df$Compound_name)
      }
      pmids <- split_semicolon(get_field(row, "pubmed_pmids", ""))

      tags$div(
        class = "fast-function-report",
        tags$div(
          class = "fast-function-hero",
          tags$h3(title),
          tags$div(
            class = "fast-function-meta",
            tags$span(class = "fast-meta-pill", icon("layer-group"), paste("Module", module_id)),
            if (!is.null(hyp_id)) {
              tags$span(class = "fast-meta-pill", icon("code-branch"), paste("Hypothesis", hyp_id))
            },
            tags$span(class = "fast-meta-pill", icon("fingerprint"), hypothesis_type),
            tags$span(class = paste("fast-meta-pill", conf_class(confidence)), icon("shield-halved"), paste("Confidence:", confidence))
          )
        ),
        tags$div(
          class = "fast-function-sections",
          # 1. The headline call: what we think this module does, the
          #    metabolites driving it, and why.
          fast_section(
            "wand-magic-sparkles",
            "Function Interpretation",
            accent = "blue",
            fast_callout(get_field(row, "functional_summary", "Not available.")),
            evidence_field(
              "Core drivers",
              chip_items = anchor_items,
              table_df = anchor_df,
              table_cols = c("anchor_top_id", "anchor_name", "Compound_ID"),
              table_labels = c(anchor_top_id = "Top ID", anchor_name = "Metabolite", Compound_ID = "Compound ID"),
              max_rows = 6,
              empty = "No anchor metabolites selected."
            )
          ),
          # 2. Chemical/biochemical evidence behind the call above.
          fast_section(
            "flask",
            "Biochemical Context",
            accent = "teal",
            fast_field(
              "Chemical-class profile",
              chip_list(split_semicolon(get_field(row, "class_names", "")), "No class profile available.")
            ),
            fast_field(
              "Pathway coverage",
              {
                groups <- list(
                  pathway_card_group("KEGG Pathways", kegg_df),
                  pathway_card_group("HMDB Pathways", hmdb_df)
                )
                groups <- groups[!vapply(groups, is.null, logical(1))]
                if (length(groups) > 0) {
                  tagList(groups)
                } else {
                  chip_list(c(
                    split_semicolon(get_field(row, "top_kegg_pathways", "")),
                    split_semicolon(get_field(row, "top_hmdb_pathways", ""))
                  ), "No pathway matches available.")
                }
              }
            ),
            fast_field(
              "Origin profile",
              origin_bar(endo_num, exo_num, unknown_num) %||% tags$p(class = "fast-muted", "No origin profile available.")
            )
          ),
          # 3. Module-level statistics: the ranked metabolite list and the
          #    overall direction of change (moved out of the hero above).
          fast_section(
            "chart-column",
            "Module Statistics",
            accent = "violet",
            fast_field(
              "Supporting metabolites",
              chip_list(utils::head(top_items, 12), "No supporting metabolite summary available.")
            ),
            fast_field("Global direction", direction_chip(direction))
          ),
          # 4. External relevance: does this line up with phenotype/literature.
          fast_section(
            "heart-pulse",
            "Phenotype & Literature",
            accent = "amber",
            fast_field("Phenotype relationship", tags$p(get_field(row, "phenotype_relationship", "–"))),
            fast_field(
              "Literature support",
              if (isTRUE(rv$function_pubmed_used)) {
                pmid_link_list(pmids)
              } else {
                tags$span(class = "fast-muted", "Literature-based annotation was disabled in this run.")
              }
            )
          ),
          # 5. How much to trust the call above.
          fast_section(
            "shield-halved",
            "Evidence Strength",
            accent = "green",
            fast_field(
              "Confidence label",
              tags$span(class = paste("fast-chip", conf_class(confidence)), confidence)
            ),
            fast_field("Rationale", tags$p(get_field(row, "rationale", "–")))
          )
        )
      )
    }

    if (!identical(rv$function_mode_used, "advanced")) {
      return(render_fast_module(mod_df[1, , drop = FALSE]))
    }

    # Advanced mode: only the accepted hypothesis chosen in the
    # "Select hypothesis" box is shown, using the same report layout as
    # fast mode.
    accepted_df <- mod_df[!is.na(mod_df$verdict_status) & mod_df$verdict_status == "accepted", , drop = FALSE]

    if (nrow(accepted_df) == 0) {
      return(
        tags$div(
          class = "empty-panel",
          icon("circle-info"),
          "No accepted hypothesis for this module."
        )
      )
    }

    sel_hyp <- input$selected_function_hypothesis
    row <- if (!is.null(sel_hyp) && sel_hyp %in% accepted_df$hypothesis_id) {
      accepted_df[as.character(accepted_df$hypothesis_id) == as.character(sel_hyp), , drop = FALSE][1, , drop = FALSE]
    } else {
      accepted_df[1, , drop = FALSE]
    }

    render_fast_module(row)
  })

  #### ---- Debate transcript (advanced mode) ----
  output$function_debate_ui <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (!identical(rv$function_mode_used, "advanced")) return(NULL)
    if (is.null(input$selected_function_module)) return(NULL)

    if (!isTRUE(rv$function_debate_html_used)) {
      return(
        tags$div(
          class = "fast-tab-placeholder",
          tags$div(class = "fast-tab-placeholder-icon", icon("comments")),
          tags$h4("Debate transcript"),
          tags$p(
            class = "fast-tab-placeholder-text",
            "Debate transcript is not available for this run. Re-run Step 5 in Advanced mode to see the ",
            "Proposer / Skeptic / Arbiter exchange here."
          )
        )
      )
    }

    debate_dir <- rv$function_debate_html_dir
    if (is.null(debate_dir) || !dir.exists(debate_dir)) return(NULL)

    module_id <- as.character(input$selected_function_module)
    fname <- paste0("debate_", gsub("[^A-Za-z0-9._-]", "_", module_id), ".html")
    fpath <- file.path(debate_dir, fname)

    if (!file.exists(fpath)) {
      tryCatch(
        export_debate_html(
          res = rv$function_result,
          module_id = module_id,
          html_dir = debate_dir
        ),
        error = function(e) NULL
      )
    }

    if (!file.exists(fpath)) {
      return(
        tags$div(
          class = "fast-tab-placeholder",
          tags$div(class = "fast-tab-placeholder-icon", icon("comments")),
          tags$h4("Debate transcript"),
          tags$p(class = "fast-tab-placeholder-text", "No debate transcript found for this module.")
        )
      )
    }

    debate_html <- tryCatch(
      paste(readLines(fpath, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      error = function(e) e
    )
    if (inherits(debate_html, "error") || !nzchar(debate_html)) {
      return(
        tags$div(
          class = "fast-tab-placeholder",
          tags$div(class = "fast-tab-placeholder-icon", icon("comments")),
          tags$h4("Debate transcript"),
          tags$p(
            class = "fast-tab-placeholder-text",
            paste0(
              "Could not read the debate transcript HTML",
              if (inherits(debate_html, "error")) paste0(": ", debate_html$message) else "."
            )
          )
        )
      )
    }

    tags$iframe(
      srcdoc = debate_html,
      class = "fast-embed-frame",
      style = "height:820px;",
      sandbox = "allow-same-origin allow-scripts"
    )
  })

  #### ---- Cross-module synthesis (advanced mode) ----
  output$function_synthesis_ui <- renderUI({
    if (rv$function_status != "finished") return(NULL)
    if (!identical(rv$function_mode_used, "advanced")) return(NULL)

    if (is.null(rv$function_result) || is.null(rv$function_result$module_synthesis)) {
      return(
        tags$div(
          class = "fast-tab-placeholder",
          tags$div(class = "fast-tab-placeholder-icon", icon("diagram-project")),
          tags$h4("Cross-module synthesis"),
          tags$p(
            class = "fast-tab-placeholder-text",
            "Cross-module synthesis was not run for this annotation. ",
            "Re-run Step 5 in Advanced mode to see the integrated network here."
          )
        )
      )
    }

    syn_path <- rv$function_synthesis_html_path
    if (is.null(syn_path) || !file.exists(syn_path)) {
      return(
        tags$div(
          class = "fast-tab-placeholder",
          tags$div(class = "fast-tab-placeholder-icon", icon("diagram-project")),
          tags$h4("Cross-module synthesis"),
          tags$p(class = "fast-tab-placeholder-text", "Synthesis completed, but the network HTML file could not be found.")
        )
      )
    }

    syn_html <- tryCatch(
      paste(readLines(syn_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      error = function(e) e
    )
    if (inherits(syn_html, "error") || !nzchar(syn_html)) {
      return(
        tags$div(
          class = "fast-tab-placeholder",
          tags$div(class = "fast-tab-placeholder-icon", icon("diagram-project")),
          tags$h4("Cross-module synthesis"),
          tags$p(
            class = "fast-tab-placeholder-text",
            paste0(
              "Could not read the synthesis network HTML",
              if (inherits(syn_html, "error")) paste0(": ", syn_html$message) else "."
            )
          )
        )
      )
    }

    tags$iframe(
      srcdoc = syn_html,
      class = "fast-embed-frame",
      style = "height:900px;",
      sandbox = "allow-same-origin allow-scripts"
    )
  })

  
  observeEvent(input$next_to_results, {
    if (!require_auth("results")) return(invisible(NULL))

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

  selected_report_export_items <- reactive({
    if (!identical(rv$function_status, "finished") ||
        !identical(rv$function_mode_used, "advanced")) {
      return(character(0))
    }
    input$report_export_items %||% character(0)
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
  outputOptions(output, "results_module_export_selector", suspendWhenHidden = FALSE)

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
    has_name <- node_group == "Metabolite" & !is.na(node_compound_name) & nzchar(node_compound_name)

    nodes <- data.frame(
      id = node_id,
      label = "",
      title = ifelse(
        has_name,
        paste0("Name: ", node_compound_name, "<br>ID: ", node_id),
        paste0("ID: ", node_id)
      ),
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
    # Exported HTML has no pre-computed coordinates, so the simulation is
    # allowed to run - but with a hard iteration cap, and physics is switched
    # off the moment it finishes so the layout stops moving.
    visNetwork(nodes, edges, width = "100%", height = "760px") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(smooth = FALSE) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        stabilization = list(
          enabled = TRUE,
          iterations = 400,
          updateInterval = 40,
          fit = TRUE
        )
      ) %>%
      visEvents(
        stabilizationIterationsDone = "function() {
          this.setOptions({ physics: false });
        }"
      )
  }

  output$results_overview <- renderUI({
    payload <- export_payload()
    module_table <- payload$module_table
    annotation_table2 <- payload$anno2
    fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

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

    mf <- payload$module_function
    score_tbl <- payload$module_score
    score_module_col <- if (is.data.frame(score_tbl) && nrow(score_tbl) > 0) {
      pick_first_col(score_tbl, c("Module", "module", "module_id"))
    } else {
      NULL
    }
    metric <- if (identical(input$func_p_type, "p")) "p" else "fdr"
    metric_col <- if (is.data.frame(score_tbl) && nrow(score_tbl) > 0) {
      if (identical(metric, "p")) {
        pick_first_col(score_tbl, c("p", "pvalue", "p_value", "P", "P.Value"))
      } else {
        pick_first_col(score_tbl, c("fdr", "FDR", "qvalue", "q_value", "padj", "p_adj"))
      }
    } else {
      NULL
    }
    cutoff <- suppressWarnings(as.numeric(input$func_p_cutoff))
    significant_modules <- if (!is.null(score_module_col) && !is.null(metric_col) &&
                               length(cutoff) == 1 && is.finite(cutoff)) {
      metric_values <- suppressWarnings(as.numeric(score_tbl[[metric_col]]))
      unique(as.character(stats::na.omit(score_tbl[[score_module_col]][metric_values < cutoff])))
    } else {
      character(0)
    }
    annotated_modules <- if (is.data.frame(mf) && nrow(mf) > 0 && "module_id" %in% names(mf)) {
      unique(as.character(stats::na.omit(mf$module_id)))
    } else {
      character(0)
    }
    annotated_significant_modules <- if (length(significant_modules) > 0) {
      intersect(annotated_modules, significant_modules)
    } else {
      annotated_modules
    }

    n_significant <- length(significant_modules)
    n_annotated_significant <- length(annotated_significant_modules)
    annotated_sig_value <- if (n_significant > 0) {
      paste0(fmt_n(n_annotated_significant), " / ", fmt_n(n_significant))
    } else {
      fmt_n(n_annotated_significant)
    }
    annotated_sig_note <- if (n_annotated_significant > 0) {
      paste0(
        toupper(metric), " < ", format(cutoff, trim = TRUE, scientific = FALSE),
        if (identical(rv$function_mode_used, "advanced")) " (Advanced)" else
          if (identical(rv$function_mode_used, "fast")) " (Fast)" else ""
      )
    } else if (identical(rv$function_status, "running")) {
      "Interpretation running"
    } else if (identical(rv$function_status, "finished")) {
      "No module passed the cutoff"
    } else {
      "Functional interpretation not run"
    }

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
        tags$div(class = "results-overview-label", "Annotated Modules"),
        tags$div(class = "results-overview-value", annotated_sig_value),
        tags$div(class = "results-overview-subtext", annotated_sig_note)
      )
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

    selected_reports <- selected_report_export_items()
    report_ready <- if (length(selected_reports) == 0) {
      TRUE
    } else {
      all(vapply(
        selected_reports,
        function(key) {
          if (identical(key, "debate")) return(isTRUE(has_debate_report()))
          if (identical(key, "synthesis")) return(length(synthesis_export_file()) > 0)
          FALSE
        },
        logical(1)
      ))
    }

    has_any_selection <-
      (length(selected) + length(selected_networks) + length(selected_reports)) > 0
    ready_for_download <- has_any_selection && table_ready && network_ready && report_ready

    shinyjs::toggleState(
      "download_results",
      condition = ready_for_download
    )
  })
  
  #### ---- Advanced-mode report exports (Results Download page) ----
  # Debate transcripts are written to disk during the Advanced run; if the
  # folder is empty they are regenerated on demand from the saved payload.
  debate_export_files <- reactive({
    debate_dir <- rv$function_debate_html_dir
    if (is.null(debate_dir) || !dir.exists(debate_dir)) return(character(0))
    files <- list.files(debate_dir, pattern = "^debate_.*\\.html?$", full.names = TRUE)
    if (length(files) == 0 && !is.null(rv$function_result)) {
      tryCatch(
        export_debate_html(res = rv$function_result, html_dir = debate_dir),
        error = function(e) NULL
      )
      files <- list.files(debate_dir, pattern = "^debate_.*\\.html?$", full.names = TRUE)
    }
    files
  })

  has_debate_report <- reactive({
    if (length(debate_export_files()) > 0) return(TRUE)
    payload <- rv$function_result$module_evidence$debate_html_payload
    !is.null(payload) && is.data.frame(payload) && nrow(payload) > 0
  })

  synthesis_export_file <- reactive({
    p <- rv$function_synthesis_html_path
    if (!is.null(p) && file.exists(p)) return(p)

    if (is.null(rv$function_result) || is.null(rv$function_result$module_synthesis)) {
      return(character(0))
    }

    html_dir <- rv$function_html_dir
    if (is.null(html_dir) || !dir.exists(html_dir)) {
      return(character(0))
    }

    candidate <- file.path(html_dir, "synthesis_network.html")
    if (!file.exists(candidate)) {
      candidate <- tryCatch(
        export_synthesis_network_html(
          res = rv$function_result,
          html_dir = html_dir,
          output_file = "synthesis_network.html",
          show_same_module = FALSE
        ),
        error = function(e) character(0)
      )
    }

    if (length(candidate) > 0 && file.exists(candidate)) {
      rv$function_synthesis_html_path <- candidate
      return(candidate)
    }

    character(0)
  })

  output$results_reports_section <- renderUI({
    if (!identical(rv$function_status, "finished") ||
        !identical(rv$function_mode_used, "advanced")) {
      return(NULL)
    }

    ready_reports <- c(
      if (isTRUE(has_debate_report())) "debate",
      if (length(synthesis_export_file()) > 0) "synthesis"
    )

    tags$div(
      class = "wb-section",
      tags$div(class = "wb-section-title", icon("file-lines"), "Reports"),
      checkboxGroupInput(
        "report_export_items",
        NULL,
        choices = c(
          "Debate transcripts" = "debate",
          "Cross-module synthesis" = "synthesis"
        ),
        selected = ready_reports
      )
    )
  })
  outputOptions(output, "results_reports_section", suspendWhenHidden = FALSE)

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("featuremap_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      if (!require_auth("results")) {
        stop("Please sign in before downloading FeatureMAP results.", call. = FALSE)
      }

      selected <- if (is.null(input$export_items)) character(0) else input$export_items
      selected_networks <- if (is.null(input$network_export_items)) character(0) else input$network_export_items
      selected_reports <- selected_report_export_items()
      if ((length(selected) + length(selected_networks) + length(selected_reports)) == 0) {
        stop("No content selected. Please select tables, networks and/or reports to export.")
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

      if ("debate" %in% selected_reports) {
        debate_files <- debate_export_files()
        if (length(debate_files) == 0) {
          stop("Debate transcripts are not available. Run Step 5 in Advanced mode first.")
        }
        file.copy(debate_files, file.path(tmpdir, basename(debate_files)), overwrite = TRUE)
      }

      if ("synthesis" %in% selected_reports) {
        syn_file <- synthesis_export_file()
        if (length(syn_file) == 0) {
          stop("The cross-module synthesis network is not available. Run Step 5 in Advanced mode first.")
        }
        file.copy(syn_file, file.path(tmpdir, "synthesis_network.html"), overwrite = TRUE)
      }

      files_to_zip <- list.files(tmpdir, full.names = TRUE)
      if (length(files_to_zip) == 0) {
        stop("No exportable content is available yet. Run PCSF/module steps or select ready tables.")
      }
      
      # ---- zip everything ----
      zip_flat(file, files_to_zip)
      
    }
    
  )
  
  # ---- run-parameter spec sheet ------------------------------------------
  rp_row <- function(label, value) {
    if (is.null(value)) return(NULL)
    value <- paste(as.character(value), collapse = " ")
    if (!nzchar(trimws(value))) return(NULL)
    tags$div(
      class = "rp-row",
      tags$span(class = "rp-label", label),
      tags$span(class = "rp-value", value)
    )
  }

  rp_card <- function(num, name, status = c("done", "pending", "skip"),
                      rows = NULL, empty = NULL) {
    status <- match.arg(status)
    status_text <- switch(status,
                          done = "Completed",
                          pending = "Not run",
                          skip = "Skipped")
    rows <- Filter(Negate(is.null), rows %||% list())
    tags$div(
      class = "rp-card",
      tags$div(
        class = "rp-head",
        tags$span(class = "rp-num", num),
        tags$span(class = "rp-name", name),
        tags$span(class = paste0("rp-status is-", status), status_text)
      ),
      if (length(rows) > 0) rows
      else tags$div(class = "rp-empty", empty %||% "No parameters recorded.")
    )
  }

  output$analysis_parameters <- renderUI({
    if (is.null(rv$feature_data)) {
      return(
        tags$p(
          class = "rp-none",
          "No analysis has been run yet. Parameters will appear here after data upload."
        )
      )
    }

    tags$div(
      class = "rp-wrap",
      tags$div(
        class = "rp-topbar",
        tags$h3(class = "rp-title", "Analysis setup")
      ),

      tags$div(
        class = "rp-grid",

        ## -------- Step 1 --------
        rp_card(
          "1", "Data & Reference", "done",
          list(
            rp_row("Reference database", rv$ref_database_type),
            rp_row("Feature table", paste(format(nrow(rv$feature_data), big.mark = ","), "features")),
            rp_row("MS2 table", if (is.null(rv$ms2_data)) "Not provided" else "Provided")
          )
        ),

        ## -------- Step 2 --------
        if (rv$annotation_status != "finished") {
          rp_card("2", "Feature Annotation", "pending",
                  empty = "Feature annotation has not been run yet.")
        } else {
          rp_card(
            "2", "Feature Annotation", "done",
            list(
              rp_row("Column type", toupper(input$column_type)),
              rp_row("MS1 tolerance", paste(input$ms1_ppm, "ppm")),
              rp_row("RT tolerance", paste(input$rt_tol, "s")),
              rp_row("Isotope number", input$isotope_num),
              rp_row("Threads", input$workers)
            )
          )
        },

        ## -------- Step 3 --------
        if (rv$pcsf_status != "finished") {
          rp_card("3", "Subnetwork (PCSF)", "pending",
                  empty = "Subnetwork extraction has not been run yet.")
        } else {
          rp_card(
            "3", "Subnetwork (PCSF)", "done",
            list(
              rp_row("Features as terminals", input$pcsf_feature),
              if (identical(input$pcsf_feature, "sig"))
                rp_row("Significance cutoff", input$pcsf_pcutoff),
              rp_row("PCSF mode",
                     if (isTRUE(input$pcsf_grid_tune)) "Grid tuning" else "Fixed parameters"),
              if (!isTRUE(input$pcsf_grid_tune))
                rp_row("b / w", paste(input$pcsf_b, "/", input$pcsf_w)),
              if (isTRUE(input$pcsf_grid_tune) && !is.null(rv$annotation_object$tuning_best))
                rp_row("Best b / w",
                       paste(rv$annotation_object$tuning_best$b, "/",
                             rv$annotation_object$tuning_best$w))
            )
          )
        },

        ## -------- Step 4 --------
        if (rv$module_status != "finished") {
          rp_card("4", "Module Identification", "pending",
                  empty = "Module identification has not been run yet.")
        } else {
          rp_card(
            "4", "Module Identification", "done",
            list(
              rp_row("Algorithm", input$module_method),
              rp_row("Node frequency threshold", input$node_freq),
              rp_row("Minimum module size", input$min_module_size),
              rp_row("Permutations", input$n_perm),
              rp_row("Random seed", input$module_seed)
            )
          )
        },

        ## -------- Step 5 --------
        if (is.null(rv$function_result)) {
          rp_card("5", "Function Interpretation", "skip",
                  empty = "Functional interpretation was skipped in this run.")
        } else {
          rp_card(
            "5", "Function Interpretation", "done",
            list(
              rp_row("Interpretation mode",
                     if (identical(input$func_mode, "advanced")) "Advanced" else "Fast"),
              rp_row("Significance cutoff",
                     paste(input$func_p_type, "<", input$func_p_cutoff)),
              rp_row("LLM provider", input$func_provider),
              rp_row("LLM model", input$func_llm_model),
              if (identical(input$func_provider, "local"))
                rp_row("Local base URL", input$func_base_url),
              rp_row("PubMed literature",
                     if (isTRUE(input$func_use_pubmed)) "Enabled" else "Disabled"),
              if (identical(input$func_mode, "advanced"))
                rp_row("Cross-module synthesis", "Enabled")
            )
          )
        }
      )
    )
  })

}

# Run the application===========================================================
shinyApp(ui = ui, server = server)

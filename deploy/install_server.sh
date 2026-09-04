#!/usr/bin/env bash
# FeatureMAP Shiny — user-level install on a server without sudo.
#
# Safe to re-run: every step skips work that is already done.
# Everything lands under $FM_ROOT; nothing outside it is touched.
#
#   bash install_server.sh              # clone/update from GitHub, then install
#   FM_REF=some-branch bash install_server.sh
set -uo pipefail

FM_ROOT="${FM_ROOT:-$HOME/featuremap-deploy}"
FM_REPO="${FM_REPO:-https://github.com/tidymass/featuremap_shiny.git}"
FM_REF="${FM_REF:-main}"
FM_SRC="$FM_ROOT/src"
FM_LIB="$FM_ROOT/rlib"
FM_BIN="$FM_ROOT/bin"
FM_LOG="$FM_ROOT/logs/install_$(date +%Y%m%d_%H%M%S).log"

# Posit Public Package Manager: precompiled binaries for Ubuntu 24.04 (noble).
# This is what lets us install without any -dev headers or sudo.
FM_CRAN="${FM_CRAN:-https://packagemanager.posit.co/cran/__linux__/noble/latest}"

mkdir -p "$FM_ROOT" "$FM_LIB" "$FM_BIN" "$FM_ROOT/logs"
exec > >(tee -a "$FM_LOG") 2>&1

step() { printf '\n\033[1m========== %s ==========\033[0m\n' "$1"; }
ok()   { printf '  \033[32m[ok]\033[0m   %s\n' "$1"; }
warn() { printf '  \033[33m[warn]\033[0m %s\n' "$1"; }
die()  { printf '  \033[31m[FAIL]\033[0m %s\n' "$1"; printf '\nLog: %s\n' "$FM_LOG"; exit 1; }

printf 'FeatureMAP Shiny installer\n  root: %s\n  log:  %s\n' "$FM_ROOT" "$FM_LOG"

# ---------------------------------------------------------------- 1. source
step "1/6  Source code"
if [ -d "$FM_SRC/.git" ]; then
  git -C "$FM_SRC" fetch --quiet origin || die "git fetch failed"
  git -C "$FM_SRC" checkout --quiet "$FM_REF" || die "no such ref: $FM_REF"
  git -C "$FM_SRC" reset --hard --quiet "origin/$FM_REF" || die "git reset failed"
  ok "updated $FM_SRC to $(git -C "$FM_SRC" log --oneline -1)"
else
  git clone --quiet --branch "$FM_REF" "$FM_REPO" "$FM_SRC" \
    || die "clone failed. If the repo is private, clone it manually into $FM_SRC and re-run."
  ok "cloned to $FM_SRC  ($(git -C "$FM_SRC" log --oneline -1))"
fi
[ -f "$FM_SRC/DESCRIPTION" ] || die "DESCRIPTION not found in $FM_SRC"

# ---------------------------------------------------------------- 2. pandoc
step "2/6  pandoc (needed by the self-contained network HTML export)"
if [ -x "$FM_BIN/pandoc" ]; then
  ok "already installed: $("$FM_BIN/pandoc" --version | head -1)"
elif command -v pandoc >/dev/null 2>&1; then
  ok "system pandoc found: $(pandoc --version | head -1)"
else
  url=$(curl -sL --max-time 30 https://api.github.com/repos/jgm/pandoc/releases/latest \
        | grep -o 'https://[^"]*linux-amd64\.tar\.gz' | head -1)
  if [ -z "$url" ]; then
    warn "could not resolve the pandoc download URL; network-graph HTML export will not work"
  else
    tmp=$(mktemp -d)
    if curl -sL --max-time 180 "$url" -o "$tmp/pandoc.tar.gz" \
       && tar xzf "$tmp/pandoc.tar.gz" -C "$tmp"; then
      found=$(find "$tmp" -type f -name pandoc -perm -u+x | head -1)
      [ -n "$found" ] && cp "$found" "$FM_BIN/pandoc" && chmod +x "$FM_BIN/pandoc" \
        && ok "installed $("$FM_BIN/pandoc" --version | head -1)" \
        || warn "pandoc binary not found inside the tarball"
    else
      warn "pandoc download failed; network-graph HTML export will not work"
    fi
    rm -rf "$tmp"
  fi
fi

# ---------------------------------------------------------------- 3. R setup
step "3/6  R library"
command -v Rscript >/dev/null 2>&1 || die "Rscript not found"
ok "$(R --version | head -1)"
ok "library: $FM_LIB"

export R_LIBS_USER="$FM_LIB"
export R_LIBS_SITE=""
NCPUS=$(( $(nproc) > 32 ? 32 : $(nproc) ))
ok "compiling with Ncpus=$NCPUS"

# The HTTPUserAgent option is what makes P3M serve binaries instead of source.
cat > "$FM_ROOT/Rprofile.install.R" <<'RPROF'
local({
  options(
    repos = c(CRAN = Sys.getenv("FM_CRAN")),
    HTTPUserAgent = sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
    ),
    Ncpus = as.integer(Sys.getenv("FM_NCPUS", "4")),
    warn = 1,
    timeout = 600
  )
})
RPROF
export FM_CRAN FM_NCPUS="$NCPUS"

runR() { R_PROFILE_USER="$FM_ROOT/Rprofile.install.R" Rscript --no-save --no-restore -e "$1"; }

# ------------------------------------------------------- 4. base tooling
step "4/6  Installing dependencies (this is the long part)"
runR 'for (p in c("remotes","BiocManager","digest")) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)
      cat("bootstrap ok\n")' || die "could not install remotes/BiocManager"
ok "remotes + BiocManager ready"

# igraph: the P3M binary links against libglpk, which is absent here. Install
# it, check it actually loads, and rebuild from source (which uses igraph's
# vendored GLPK) if it does not.
runR '
  if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
  loaded <- tryCatch({ loadNamespace("igraph"); TRUE }, error = function(e) {
    cat("binary igraph failed to load:", conditionMessage(e), "\n"); FALSE })
  if (!loaded) {
    cat("--> rebuilding igraph from source (vendored GLPK)\n")
    install.packages("igraph", type = "source")
    loadNamespace("igraph")
  }
  cat("igraph ok:", as.character(packageVersion("igraph")), "\n")
' || die "igraph could not be installed"
ok "igraph works"

# Rdisop lives on Bioconductor, not CRAN.
runR 'if (!requireNamespace("Rdisop", quietly=TRUE)) BiocManager::install("Rdisop", ask=FALSE, update=FALSE)
      loadNamespace("Rdisop"); cat("Rdisop ok\n")' || die "Rdisop (Bioconductor) failed"
ok "Rdisop works"

# Everything else, including the tidymass packages listed under Remotes:
runR "remotes::install_deps('$FM_SRC', dependencies = TRUE, upgrade = 'never')
      cat('deps done\n')" || die "dependency install failed — see the log above"
ok "dependencies installed"

# ---------------------------------------------------------------- 5. package
step "5/6  Installing featuremapshiny"
R_LIBS_USER="$FM_LIB" R CMD INSTALL --no-multiarch --with-keep.source "$FM_SRC" \
  || die "R CMD INSTALL failed"
ok "featuremapshiny installed"

# ---------------------------------------------------------------- 6. verify
step "6/6  Verification"
runR '
  pkgs <- c("shiny","shinyjs","DT","plotly","visNetwork","bslib","dplyr","tibble",
            "tidyr","stringr","magrittr","R6","future","furrr","igraph","tidygraph",
            "purrr","glue","jsonlite","data.table","plyr","MASS","httr","rentrez",
            "xml2","mclust","openxlsx","htmlwidgets","scales","massdataset","metid",
            "pcsf2featuremap","Rdisop","masstools","rlang","cli","digest",
            "featuremapshiny")
  bad <- character(0)
  for (p in pkgs) {
    okp <- tryCatch({ loadNamespace(p); TRUE }, error = function(e) FALSE)
    cat(sprintf("  %-18s %s\n", p, if (okp) as.character(packageVersion(p)) else "*** FAILED ***"))
    if (!okp) bad <- c(bad, p)
  }
  cat("\napp dir: ", system.file("app", package = "featuremapshiny"), "\n", sep = "")
  if (length(bad)) { cat("\nFAILED:", paste(bad, collapse=", "), "\n"); quit(status = 1) }
  cat("\nAll packages load.\n")
'
verdict=$?

# Environment file used by the run script.
cat > "$FM_ROOT/env.sh" <<ENVF
# Sourced by run_app.sh. Generated by install_server.sh on $(date).
export FM_ROOT="$FM_ROOT"
export R_LIBS_USER="$FM_LIB"
export PATH="$FM_BIN:\$PATH"
ENVF
ok "wrote $FM_ROOT/env.sh"

if [ $verdict -eq 0 ]; then
  printf '\n\033[32m=== install complete ===\033[0m\nLog: %s\n\n' "$FM_LOG"
else
  printf '\n\033[31m=== some packages failed to load ===\033[0m\nLog: %s\n' "$FM_LOG"
  printf 'Send me the failing names and the matching lines from the log.\n\n'
fi

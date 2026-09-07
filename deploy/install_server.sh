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
FM_LOCAL="$FM_ROOT/local"      # prefix for C libraries we build ourselves
FM_LOG="$FM_ROOT/logs/install_$(date +%Y%m%d_%H%M%S).log"

# Posit Public Package Manager: precompiled binaries for Ubuntu 24.04 (noble).
# This is what lets us install without any -dev headers or sudo.
FM_CRAN="${FM_CRAN:-https://packagemanager.posit.co/cran/__linux__/noble/latest}"

mkdir -p "$FM_ROOT" "$FM_LIB" "$FM_BIN" "$FM_ROOT/logs" "$FM_LOCAL"
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

NCPUS=$(( $(nproc) > 32 ? 32 : $(nproc) ))
ok "compiling with Ncpus=$NCPUS"

# This machine sets R_LIBS_USER in Renviron.site (pointing at /scratch), and a
# hard assignment there overrides anything we export. So the library location
# is forced two ways that Renviron.site cannot undo: .libPaths() inside
# R_PROFILE_USER (profiles run after Renviron) and an explicit -l on
# R CMD INSTALL.
# Renviron.site on this machine hard-assigns R_LIBS_USER to /scratch, which
# overrides anything we export. R reads the SITE env file first and the USER
# env file second, so a user env file wins -- and pointing at ours via
# R_ENVIRON_USER keeps it scoped to this script instead of touching
# ~/.Renviron, which would redirect the user's own R sessions too.
#
# This matters beyond the parent process: remotes builds source packages in a
# child `R CMD INSTALL`, and that child resolves LinkingTo headers (Rcpp.h,
# boost/) through R_LIBS/R_LIBS_USER -- not through the .libPaths() we set in
# the profile. Without this, packages install into rlib but their headers are
# looked up in /scratch.
cat > "$FM_ROOT/Renviron.install" <<ENVR
R_LIBS=$FM_LIB
R_LIBS_USER=$FM_LIB
ENVR
export R_ENVIRON_USER="$FM_ROOT/Renviron.install"

export R_LIBS_USER="$FM_LIB"
export R_LIBS="$FM_LIB"
export FM_LIB FM_LOCAL FM_CRAN
export FM_NCPUS="$NCPUS"

# Our own C libraries must be found both when compiling and at run time.
export PKG_CONFIG_PATH="$FM_LOCAL/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
export LD_LIBRARY_PATH="$FM_LOCAL/lib:${LD_LIBRARY_PATH:-}"
export CPPFLAGS="-I$FM_LOCAL/include ${CPPFLAGS:-}"
export LDFLAGS="-L$FM_LOCAL/lib -Wl,-rpath,$FM_LOCAL/lib ${LDFLAGS:-}"

# The HTTPUserAgent option is what makes P3M serve binaries instead of source.
cat > "$FM_ROOT/Rprofile.install.R" <<'RPROF'
local({
  lib <- Sys.getenv("FM_LIB")
  if (nzchar(lib)) {
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, .libPaths()))
  }
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

runR() { R_PROFILE_USER="$FM_ROOT/Rprofile.install.R" Rscript --no-save --no-restore -e "$1"; }

# ---- GLPK -------------------------------------------------------------
# igraph links against libglpk. It is absent here and we cannot apt-install
# it, so build it into $FM_LOCAL. Small, pure C, about a minute.
if [ -f "$FM_LOCAL/lib/libglpk.so" ] || ldconfig -p 2>/dev/null | grep -q libglpk; then
  ok "glpk already available"
else
  step "3b   Building GLPK into $FM_LOCAL (no sudo needed)"
  command -v gcc  >/dev/null 2>&1 || die "gcc not found — cannot build GLPK"
  command -v make >/dev/null 2>&1 || die "make not found — cannot build GLPK"
  gtmp=$(mktemp -d)
  if curl -sL --max-time 300 https://ftp.gnu.org/gnu/glpk/glpk-5.0.tar.gz -o "$gtmp/glpk.tar.gz" \
     && tar xzf "$gtmp/glpk.tar.gz" -C "$gtmp"; then
    ( cd "$gtmp/glpk-5.0" \
      && ./configure --prefix="$FM_LOCAL" --quiet \
      && make -j"$NCPUS" -s \
      && make install -s ) || die "GLPK build failed — see the log"
    [ -f "$FM_LOCAL/lib/libglpk.so" ] || die "GLPK built but libglpk.so is missing"
    ok "GLPK installed into $FM_LOCAL"
  else
    die "could not download GLPK from ftp.gnu.org"
  fi
  rm -rf "$gtmp"
fi

# ------------------------------------------------------- 4. base tooling
step "4/6  Installing dependencies (this is the long part)"
# Rcpp and BH are LinkingTo (compile-time only) dependencies of
# pcsf2featuremap. Nothing imports them at run time, so they can be missed
# when resolving a chain of remote dependencies -- and their absence shows up
# only as "fatal error: boost/....hpp: No such file or directory" deep in a
# compile log. Install them up front.
runR 'cat("parent .libPaths()[1]: ", .libPaths()[1], "\n", sep="")'
Rscript -e 'cat("child  .libPaths()[1]: ", .libPaths()[1], "\n", sep="")'

runR 'lib <- .libPaths()[1]
      for (p in c("remotes","BiocManager","digest"))
        if (!requireNamespace(p, quietly=TRUE)) install.packages(p, lib = lib)
      cat("bootstrap ok\n")' || die "could not install the bootstrap packages"
ok "remotes + BiocManager ready"

# Rcpp and BH are header-only from the compilers point of view. Being
# *loadable* is not enough and is not even the right test: R CMD INSTALL
# resolves LinkingTo to <lib>/<pkg>/include, so the headers must exist in a
# library this build can see. A system copy that satisfies requireNamespace()
# but ships no headers would silently skip the install and fail later with
# "fatal error: Rcpp.h: No such file or directory".
runR '
  lib <- .libPaths()[1]
  need <- character(0)
  for (p in c("Rcpp","BH")) {
    hdr <- if (p == "Rcpp") "Rcpp.h" else file.path("boost","config.hpp")
    found <- FALSE
    for (l in .libPaths()) {
      f <- file.path(l, p, "include", hdr)
      if (file.exists(f)) { cat(sprintf("  %-5s headers: %s\n", p, f)); found <- TRUE; break }
    }
    if (!found) need <- c(need, p)
  }
  if (length(need)) {
    cat("--> installing headers for:", paste(need, collapse=", "), "\n")
    install.packages(need, lib = lib)
  }
  bad <- character(0)
  for (p in c("Rcpp","BH")) {
    hdr <- if (p == "Rcpp") "Rcpp.h" else file.path("boost","config.hpp")
    if (!any(file.exists(file.path(.libPaths(), p, "include", hdr)))) bad <- c(bad, p)
  }
  if (length(bad)) { cat("MISSING HEADERS:", paste(bad, collapse=", "), "\n"); quit(status = 1) }
  cat("headers ok\n")
' || die "Rcpp/BH headers are not available — pcsf2featuremap cannot compile without them"
ok "Rcpp + BH headers in place"

# igraph: the P3M binary links against libglpk, which is absent here. Install
# it, check it actually loads, and rebuild from source (which uses igraph's
# vendored GLPK) if it does not.
runR '
  lib <- .libPaths()[1]
  cat("installing into: ", lib, "\n", sep = "")

  # A fresh Rscript each time, so no unloading is needed. Report WHY it fails:
  # a missing dependency and a missing shared library need different fixes.
  loads <- function() tryCatch({ loadNamespace("igraph"); TRUE },
    error = function(e) { cat("  igraph does not load: ", conditionMessage(e), "\n", sep = ""); FALSE })

  # An existing igraph directory does not mean its dependencies are present,
  # so never skip straight to the load test on that basis.
  if (!loads()) {
    cat("--> installing igraph and its dependencies\n")
    install.packages("igraph", lib = lib)
  }
  if (!loads()) {
    cat("--> rebuilding igraph from source against our GLPK\n")
    install.packages("igraph", lib = lib, type = "source")
  }
  loadNamespace("igraph")
  cat("igraph ok: ", as.character(packageVersion("igraph")),
      "  from ", dirname(getNamespaceInfo("igraph", "path")), "\n", sep = "")
' || die "igraph could not be installed"
ok "igraph works"

# Rdisop lives on Bioconductor, not CRAN.
runR 'ok <- tryCatch({ loadNamespace("Rdisop"); TRUE }, error = function(e) FALSE)
      if (!ok) BiocManager::install("Rdisop", ask=FALSE, update=FALSE, lib = .libPaths()[1])
      loadNamespace("Rdisop"); cat("Rdisop ok\n")' || die "Rdisop (Bioconductor) failed"
ok "Rdisop works"

# Everything else, including the tidymass packages listed under Remotes:
runR "remotes::install_deps('$FM_SRC', dependencies = TRUE, upgrade = 'never')
      cat('deps done\n')" || die "dependency install failed — see the log above"
ok "dependencies installed"

# pcsf2featuremap compiles C++ against Boost; verify it separately so a build
# failure here is not mistaken for a problem with featuremapshiny itself.
runR '
  if (!requireNamespace("pcsf2featuremap", quietly = TRUE)) {
    cat("--> pcsf2featuremap missing, installing it directly\n")
    remotes::install_github("tidymass/pcsf2featuremap",
                            lib = .libPaths()[1], upgrade = "never")
  }
  loadNamespace("pcsf2featuremap")
  cat("pcsf2featuremap ok:", as.character(packageVersion("pcsf2featuremap")), "\n")
' || die "pcsf2featuremap failed to build — check the log for the first 'fatal error:' line"
ok "pcsf2featuremap works"

# ---------------------------------------------------------------- 5. package
step "5/6  Installing featuremapshiny"
R CMD INSTALL -l "$FM_LIB" --no-multiarch --with-keep.source "$FM_SRC" \
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

# Environment file used by the run script. Preserve existing private secrets
# such as FEATUREMAP_SMTP_PASSWORD when install_server.sh is re-run.
if [ -f "$FM_ROOT/env.sh" ]; then
  ok "kept existing $FM_ROOT/env.sh"
else
  cat > "$FM_ROOT/env.sh" <<ENVF
# Sourced by run_app.sh. Generated by install_server.sh on $(date).
export FM_ROOT="$FM_ROOT"
export FM_LIB="$FM_LIB"
export R_LIBS_USER="$FM_LIB"
export LD_LIBRARY_PATH="$FM_LOCAL/lib:\${LD_LIBRARY_PATH:-}"
export PATH="$FM_BIN:\$PATH"
ENVF
  ok "wrote $FM_ROOT/env.sh"
fi

if [ $verdict -eq 0 ]; then
  printf '\n\033[32m=== install complete ===\033[0m\nLog: %s\n\n' "$FM_LOG"
else
  printf '\n\033[31m=== some packages failed to load ===\033[0m\nLog: %s\n' "$FM_LOG"
  printf 'Send me the failing names and the matching lines from the log.\n\n'
fi

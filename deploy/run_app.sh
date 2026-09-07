#!/usr/bin/env bash
# Start / stop / update the deployed FeatureMAP Shiny app.
#
#   ./run_app.sh start     launch in the background (survives logout)
#   ./run_app.sh stop      shut it down
#   ./run_app.sh restart   stop, then start
#   ./run_app.sh status    is it running, and on what URL
#   ./run_app.sh logs      follow the live log
#   ./run_app.sh update    git pull + reinstall + restart
#
# Uses nohup rather than tmux/screen: neither is installed here and without
# sudo we cannot add them. nohup needs nothing beyond a POSIX shell.
set -uo pipefail

FM_ROOT="${FM_ROOT:-$HOME/featuremap-deploy}"
FM_SRC="$FM_ROOT/src"
FM_LIB="$FM_ROOT/rlib"
FM_LOCAL="$FM_ROOT/local"
FM_BIN="$FM_ROOT/bin"
FM_ENV="$FM_ROOT/env.sh"

# Private deployment secrets/configuration live outside git.
# Example: FEATUREMAP_SMTP_PASSWORD for Gmail app-password SMTP.
if [ -f "$FM_ENV" ]; then
  # shellcheck disable=SC1090
  . "$FM_ENV"
fi

# ---- deployment settings -------------------------------------------------
export FM_HOST="${FM_HOST:-0.0.0.0}"
export FM_PORT="${FM_PORT:-3838}"
export FEATUREMAP_MAX_UPLOAD_MB="${FEATUREMAP_MAX_UPLOAD_MB:-500}"
export FEATUREMAP_AUTH_ENABLED="${FEATUREMAP_AUTH_ENABLED:-false}"
export FEATUREMAP_AUTH_DB="${FEATUREMAP_AUTH_DB:-$FM_ROOT/auth/featuremap_auth.sqlite}"
export FEATUREMAP_PUBLIC_URL="${FEATUREMAP_PUBLIC_URL:-}"
export FEATUREMAP_SMTP_HOST="${FEATUREMAP_SMTP_HOST:-smtp.gmail.com}"
export FEATUREMAP_SMTP_PORT="${FEATUREMAP_SMTP_PORT:-587}"
export FEATUREMAP_SMTP_USERNAME="${FEATUREMAP_SMTP_USERNAME:-featuremapofficial@gmail.com}"
export FEATUREMAP_SMTP_FROM="${FEATUREMAP_SMTP_FROM:-featuremapofficial@gmail.com}"
# export FEATUREMAP_SMTP_PASSWORD=your-gmail-app-password
# export SILICONFLOW_BASE_URL=https://api.siliconflow.cn   # pin one endpoint
# --------------------------------------------------------------------------

export FM_ROOT FM_LIB
export R_ENVIRON_USER="$FM_ROOT/Renviron.install"
export R_LIBS="$FM_LIB"
export R_LIBS_USER="$FM_LIB"
export LD_LIBRARY_PATH="$FM_LOCAL/lib:${LD_LIBRARY_PATH:-}"
export PATH="$FM_BIN:$PATH"

LOG="$FM_ROOT/logs/app.log"
PIDFILE="$FM_ROOT/app.pid"
mkdir -p "$FM_ROOT/logs"

ip_guess() { ip -4 addr show 2>/dev/null | awk '/inet /&&!/127\.0\.0\.1/{sub(/\/.*/,"",$2); print $2; exit}'; }

pid_of() { [ -f "$PIDFILE" ] && cat "$PIDFILE" 2>/dev/null; }
running() {
  local p; p=$(pid_of) || return 1
  [ -n "${p:-}" ] && kill -0 "$p" 2>/dev/null
}

case "${1:-status}" in

  start)
    if running; then echo "Already running (pid $(pid_of)). Use restart to reload."; exit 0; fi
    [ -d "$FM_LIB/featuremapshiny" ] || { echo "featuremapshiny is not installed. Run install_server.sh first."; exit 1; }

    # Fail with a clear message rather than letting R bind and die.
    if ss -ltn 2>/dev/null | grep -q ":$FM_PORT "; then
      echo "Port $FM_PORT is already in use. Set FM_PORT to something else."; exit 1
    fi

    [ -f "$LOG" ] && mv -f "$LOG" "$LOG.$(date +%Y%m%d_%H%M%S)"
    nohup Rscript "$FM_SRC/deploy/run_app.R" >> "$LOG" 2>&1 < /dev/null &
    echo $! > "$PIDFILE"
    disown 2>/dev/null || true

    sleep 5
    if running; then
      echo "Started (pid $(pid_of))."
      echo "  URL for your group:  http://$(ip_guess):$FM_PORT"
      echo "  Log:                 $LOG"
      echo
      tail -n 15 "$LOG" 2>/dev/null
    else
      echo "Failed to start. Last lines of the log:"; tail -n 30 "$LOG" 2>/dev/null; rm -f "$PIDFILE"; exit 1
    fi
    ;;

  stop)
    if running; then
      p=$(pid_of); kill "$p" 2>/dev/null
      for _ in $(seq 20); do running || break; sleep 0.5; done
      # future/furrr may leave workers behind; clear any stragglers.
      running && kill -9 "$p" 2>/dev/null
      pkill -f "$FM_SRC/deploy/run_app.R" 2>/dev/null
      rm -f "$PIDFILE"
      echo "Stopped."
    else
      rm -f "$PIDFILE"; echo "Not running."
    fi
    ;;

  restart) "$0" stop; sleep 1; "$0" start ;;

  status)
    if running; then
      p=$(pid_of)
      echo "RUNNING   http://$(ip_guess):$FM_PORT"
      echo "  pid          : $p  (up $(ps -o etime= -p "$p" 2>/dev/null | tr -d ' '))"
      echo "  version      : $(Rscript -e 'cat(as.character(packageVersion("featuremapshiny")))' 2>/dev/null)"
      echo "  source commit: $(git -C "$FM_SRC" log --oneline -1 2>/dev/null)"
      echo "  log          : $LOG"
    else
      echo "NOT RUNNING"
    fi
    ;;

  logs) tail -f "$LOG" ;;

  update)
    echo "== pulling =="
    git -C "$FM_SRC" pull --ff-only || { echo "git pull failed"; exit 1; }
    echo "== reinstalling =="
    # Dependencies may have changed (a new Imports/Suggests entry), so refresh
    # them before installing the package itself.
    R_PROFILE_USER="$FM_ROOT/Rprofile.install.R" \
    FM_CRAN="${FM_CRAN:-https://packagemanager.posit.co/cran/__linux__/noble/latest}" \
      Rscript -e "remotes::install_deps('$FM_SRC', dependencies = TRUE, upgrade = 'never')" || exit 1
    R CMD INSTALL -l "$FM_LIB" --no-multiarch --with-keep.source "$FM_SRC" || exit 1
    echo "== restarting =="
    # Required: the running R process holds the old code in memory and keeps
    # serving it until it is replaced.
    "$0" restart
    ;;

  *) echo "usage: $0 {start|stop|restart|status|logs|update}"; exit 1 ;;
esac

#!/usr/bin/env bash
# FeatureMAP Shiny — server readiness check.
# Read-only: installs nothing, changes nothing, needs no sudo.
# Usage:  bash check_server.sh

say() { printf '\n\033[1m== %s ==\033[0m\n' "$1"; }
ok()  { printf '  \033[32m[ok]\033[0m   %s\n' "$1"; }
no()  { printf '  \033[31m[MISS]\033[0m %s\n' "$1"; }
inf() { printf '  [info] %s\n' "$1"; }

say "1. Identity and privileges"
inf "user: $(whoami)   host: $(hostname)"
if sudo -n true 2>/dev/null; then
  ok "passwordless sudo available"
elif sudo -v 2>/dev/null; then
  ok "sudo available (asks for password)"
else
  no "no sudo — plan for a user-level install"
fi

say "2. OS"
if [ -f /etc/os-release ]; then . /etc/os-release; inf "$PRETTY_NAME"; else inf "$(uname -a)"; fi
inf "kernel: $(uname -r)"

say "3. Hardware"
inf "CPU cores: $(nproc 2>/dev/null || echo '?')"
inf "RAM: $(free -h 2>/dev/null | awk '/^Mem:/{print $2" total, "$7" available"}' || echo '?')"
inf "disk (home): $(df -h "$HOME" 2>/dev/null | awk 'NR==2{print $4" free of "$2}')"
if command -v nvidia-smi >/dev/null 2>&1; then
  ok "GPU present:"
  nvidia-smi --query-gpu=name,memory.total,memory.used --format=csv,noheader 2>/dev/null | sed 's/^/         /'
else
  inf "no nvidia-smi (no GPU, or drivers not on PATH)"
fi

say "4. R"
if command -v Rscript >/dev/null 2>&1; then
  ok "$(R --version 2>/dev/null | head -1)"
  inf "Rscript: $(command -v Rscript)"
  echo '  [info] .libPaths():'
  Rscript -e 'cat(paste0("         ", .libPaths(), " (writable: ", file.access(.libPaths(), 2) == 0, ")\n"))' 2>/dev/null
  echo '  [info] key packages already installed:'
  Rscript -e 'for (p in c("shiny","igraph","xml2","httr","openxlsx","htmlwidgets","devtools","remotes","digest")) cat(sprintf("         %-13s %s\n", p, if (requireNamespace(p, quietly=TRUE)) as.character(packageVersion(p)) else "MISSING"))' 2>/dev/null
else
  no "R is not installed (or not on PATH)"
fi

say "5. Command-line tools the app needs at runtime"
for c in zip unzip pandoc git curl; do
  if command -v "$c" >/dev/null 2>&1; then ok "$c  -> $(command -v "$c")"; else no "$c  MISSING"; fi
done
inf "zip is needed by every download button; pandoc by the self-contained network HTML export"

say "6. System libraries needed to COMPILE the R dependencies"
for lib in libcurl libxml-2.0 openssl glpk libgit2; do
  if pkg-config --exists "$lib" 2>/dev/null; then
    ok "$lib $(pkg-config --modversion "$lib" 2>/dev/null)"
  else
    no "$lib  (dev headers not found via pkg-config)"
  fi
done
inf "glpk is what igraph needs; missing ones only matter if the package must build from source"

say "7. Container runtime"
if command -v docker >/dev/null 2>&1; then
  if docker info >/dev/null 2>&1; then ok "docker usable as $(whoami): $(docker --version)";
  else no "docker installed but not usable by this user (needs the docker group or sudo)"; fi
else
  inf "docker not installed"
fi
command -v singularity >/dev/null 2>&1 && ok "singularity: $(singularity --version)"
command -v apptainer   >/dev/null 2>&1 && ok "apptainer: $(apptainer --version)"

say "8. Outbound network"
for url in https://cloud.r-project.org https://github.com https://gitlab.com https://api.siliconflow.cn https://api.siliconflow.com; do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 8 "$url" 2>/dev/null)
  if [ "$code" != "000" ] && [ -n "$code" ]; then ok "$url  (HTTP $code)"; else no "$url  unreachable"; fi
done

say "9. Where users would connect"
inf "hostname -f: $(hostname -f 2>/dev/null || hostname)"
inf "addresses:"
{ ip -4 addr show 2>/dev/null | awk '/inet /{print "         "$2"  ("$NF")"}'; } || \
{ ifconfig 2>/dev/null | awk '/inet /{print "         "$2}'; }
for p in 3838 8080; do
  if command -v ss >/dev/null 2>&1; then busy=$(ss -ltn "sport = :$p" 2>/dev/null | grep -c LISTEN)
  else busy=$(netstat -ltn 2>/dev/null | grep -c ":$p "); fi
  if [ "${busy:-0}" -gt 0 ]; then no "port $p already in use"; else ok "port $p free"; fi
done
if command -v firewall-cmd >/dev/null 2>&1; then inf "firewalld: $(firewall-cmd --state 2>/dev/null)"; fi
if command -v ufw >/dev/null 2>&1; then inf "ufw: $(ufw status 2>/dev/null | head -1)"; fi

printf '\n\033[1m== done ==\033[0m\nSend the whole output back.\n\n'

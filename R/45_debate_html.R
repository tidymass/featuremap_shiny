# ============================================================
# 45_debate_html.R
# Debate HTML visualizer - academic chat-style layout
#
# Sourced automatically by 45_advanced_mode.R.
# Public export: generate_debate_html()
# ============================================================


# -- Internal helpers ------------------------------------------

.he <- function(s) {
  s <- as.character(s %||% "")
  s <- gsub("&",  "&amp;",  s, fixed = TRUE)
  s <- gsub("<",  "&lt;",   s, fixed = TRUE)
  s <- gsub(">",  "&gt;",   s, fixed = TRUE)
  s <- gsub('"',  "&quot;", s, fixed = TRUE)
  s
}

# Coerce any list/vector field to a single character scalar (prevents
# "condition has length > 1" when JSON parsing returns unexpected vectors)
.s <- function(x, default = "") {
  x <- x %||% default
  x <- as.character(x)
  if (length(x) == 0L) return(as.character(default)[1L])
  x <- trimws(x[1L])
  if (is.na(x) || !nzchar(x) || toupper(x) %in% c("NA", "NULL")) {
    return(as.character(default)[1L])
  }
  x
}

.is_nonempty <- function(x) {
  x <- .s(x, "")
  nzchar(x)
}

# Truncate long string(s), appending ellipsis.  Vectorised - safe for
# character vectors (e.g. when called from .met_chips() with multiple names).
.trunc <- function(s, n) {
  s <- as.character(s %||% "")
  ifelse(nchar(s) > n, paste0(substr(s, 1L, n - 1L), "..."), s)
}

# Render all metabolite chips
.met_chips <- function(names) {
  names <- unique(trimws(as.character(names %||% character(0))))
  names <- names[nzchar(names) & !is.na(names) & toupper(names) != "NA"]
  if (length(names) == 0L) return('<span class="muted">-</span>')
  chips <- paste(
    sprintf('<span class="met-chip" title="%s">%s</span>',
            .he(names), .he(names)),
    collapse = "")
  chips
}

# Render top class profile from precomputed Class-level structure
.class_bars <- function(module_obj, n = 3L) {
  df <- module_obj$other_metabolites
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
      !"class" %in% colnames(df) || !"n_metabolites" %in% colnames(df)) {
    return('<span class="muted">-</span>')
  }

  cls <- trimws(as.character(df$class))
  keep <- !is.na(cls) & nzchar(cls) & !toupper(cls) %in% c("NA", "NULL", "UNKNOWN")
  df <- df[keep, , drop = FALSE]
  cls <- cls[keep]
  if (nrow(df) == 0L) return('<span class="muted">-</span>')
  df$class <- cls
  df$n_metabolites <- suppressWarnings(as.numeric(df$n_metabolites))
  df <- df[is.finite(df$n_metabolites) & df$n_metabolites > 0, , drop = FALSE]
  if (nrow(df) == 0L) return('<span class="muted">-</span>')

  df <- df[order(-df$n_metabolites), , drop = FALSE]
  rows <- head(df, n)
  bars <- paste(vapply(seq_len(nrow(rows)), function(i) {
    pct <- round(rows$n_metabolites[[i]] * 100)
    lbl <- rows$class[[i]]
    sprintf(
      '<div class="cls-row">
        <div class="cls-name">%s</div>
        <div class="cls-track"><div class="cls-fill" style="width:%d%%"></div></div>
        <div class="cls-pct">%d%%</div>
      </div>',
      .he(lbl), pct, pct)
  }, character(1L)), collapse = "\n")

  sprintf('<div class="cls-block">%s</div>', bars)
}

# Render anchor / tension metabolite tags (subtle, dot-prefixed)
.met_tags <- function(csv, variant) {
  # variant: "anc" | "ten"
  vals <- trimws(strsplit(as.character(csv %||% ""), ",")[[1L]])
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0L) return("")
  label <- if (identical(variant, "anc")) "Core anchors" else "Tension metabolites"
  tags  <- paste(
    sprintf('<span class="mtag mtag-%s">%s</span>', variant, .he(vals)),
    collapse = "")
  sprintf('<div class="mtag-row"><span class="mtag-label">%s</span>%s</div>',
          label, tags)
}

# Render one hypothesis card
.hyp_card <- function(h) {
  anc <- paste(as.character(h$anchor_metabolites  %||% character(0)), collapse = ", ")
  ten <- paste(as.character(h$tension_metabolites %||% character(0)), collapse = ", ")
  typ <- .s(h$type, "")
  type_html <- if (nzchar(typ))
    sprintf('<span class="type-pill">%s</span>', .he(typ)) else ""
  sprintf(
    '<div class="hyp-card">
      <div class="hyp-header">
        <span class="hyp-id">%s</span>
        <span class="hyp-title">%s</span>
        %s
      </div>
      <div class="hyp-summary">%s</div>
      %s%s
    </div>',
    .he(.s(h$hypothesis_id)),
    .he(.s(h$title)),
    type_html,
    .he(.s(h$summary)),
    .met_tags(anc, "anc"),
    .met_tags(ten, "ten")
  )
}

# Render one Skeptic challenge
.ch_item <- function(ch) {
  sev   <- .s(ch$severity,       "minor")
  ctype <- .s(ch$challenge_type, "")
  if (identical(sev, "no_challenge")) return("")
  sev_cls <- if (identical(sev, "major")) "sev-major" else "sev-minor"
  sprintf(
    '<div class="ch-item ch-%s">
      <div class="ch-meta">
        <span class="sev-pill %s">%s</span>
        <span class="ch-type">%s</span>
        <span class="ch-target">%s</span>
      </div>
      <div class="ch-text">%s</div>
    </div>',
    sev, sev_cls, .he(toupper(sev)),
    .he(ctype),
    .he(.s(ch$target_hypothesis_id)),
    .he(.s(ch$challenge))
  )
}

# Render one Proposer revision
.rev_item <- function(rev, hypotheses_after) {
  hid    <- .s(rev$hypothesis_id)
  action <- toupper(.s(rev$action, "keep"))
  just   <- .s(rev$justification)
  h_new  <- Filter(function(x) x$hypothesis_id == hid, hypotheses_after)

  act_cls <- switch(action, MODIFY = "act-modify", DROP = "act-drop", "act-keep")

  detail <- ""
  if (identical(action, "MODIFY") && length(h_new) > 0L) {
    h   <- h_new[[1L]]
    anc <- paste(as.character(h$anchor_metabolites  %||% character(0)), collapse = ", ")
    ten <- paste(as.character(h$tension_metabolites %||% character(0)), collapse = ", ")
    typ <- .s(h$type, "")
    type_html <- if (nzchar(typ))
      sprintf('<div class="rev-new-type">%s</div>', .he(typ)) else ""
    detail <- sprintf(
      '<div class="rev-detail">
        %s
        <div class="rev-new-title">%s</div>
        <div class="rev-new-summary">%s</div>
        %s%s
      </div>',
      type_html,
      .he(.s(h$title)),
      .he(.s(h$summary)),
      .met_tags(anc, "anc"),
      .met_tags(ten, "ten")
    )
  }
  sprintf(
    '<div class="rev-item">
      <div class="rev-header">
        <span class="rev-hid">%s</span>
        <span class="act-pill %s">%s</span>
      </div>
      <div class="rev-just">%s</div>
      %s
    </div>',
    .he(hid), act_cls, action, .he(just), detail
  )
}

# Render one Arbiter verdict card
.verdict_card <- function(v, hypotheses_final) {
  hid    <- .s(v$hypothesis_id)
  status <- .s(v$status,               "rejected")
  conf   <- .s(v$confidence_label,     "")
  vrat   <- .s(v$verdict_rationale,    "")
  crat   <- .s(v$confidence_rationale, "")
  st_cls <- switch(status,
                   accepted  = "st-accepted",
                   "st-rejected")
  h_fin       <- Filter(function(x) x$hypothesis_id == hid, hypotheses_final %||% list())
  fin_title   <- if (length(h_fin) > 0L) .s(h_fin[[1L]]$title)   else ""
  fin_summary <- if (length(h_fin) > 0L) .s(h_fin[[1L]]$summary) else ""
  fin_type    <- if (length(h_fin) > 0L) .s(h_fin[[1L]]$type)    else ""

  status_pill <- sprintf(
    '<span class="status-pill %s">%s</span>', st_cls, .he(toupper(status)))
  conf_pill <- if (nzchar(conf))
    sprintf('<span class="conf-pill conf-%s">%s confidence</span>', .he(conf), .he(conf))
  else ""

  title_html <- if (nzchar(fin_title))
    sprintf('<div class="verdict-title">%s</div>', .he(fin_title)) else ""
  type_html <- if (nzchar(fin_type))
    sprintf('<div class="verdict-type">%s</div>', .he(fin_type)) else ""
  summary_html <- if (nzchar(fin_summary))
    sprintf('<div class="verdict-fin-summary">%s</div>', .he(fin_summary)) else ""

  # Verdict rationale block
  vrat_html <- if (nzchar(vrat)) sprintf(
    '<div class="rat-block">
      <div class="rat-label">Verdict rationale</div>
      <div class="rat-text">%s</div>
    </div>', .he(vrat)) else ""

  # Confidence rationale block - with divider when verdict block also present
  crat_html <- if (nzchar(crat)) sprintf(
    '%s<div class="rat-block">
      <div class="rat-label">Confidence rationale
        %s
      </div>
      <div class="rat-text">%s</div>
    </div>',
    if (nzchar(vrat)) '<div class="rat-divider"></div>' else "",
    conf_pill,
    .he(crat)
  ) else ""

  sprintf(
    '<div class="verdict-card %s">
      <div class="verdict-hdr">
        <span class="verdict-hid">%s</span>
        %s
      </div>
      %s%s%s
      %s%s
    </div>',
    st_cls,
    .he(hid), status_pill,
    type_html, title_html, summary_html,
    vrat_html, crat_html
  )
}

# Render one PubMed phenotype-evidence card
.pubmed_card <- function(hid, rel = NULL, evidence = NULL) {
  support <- .s(rel$literature_support, "insufficient")
  relationship <- .s(rel$phenotype_relationship, "")
  note <- .s(rel$evidence_note, "")

  pmids <- as.character(evidence$pmids %||% character(0))
  pmids <- unique(trimws(pmids))
  pmids <- pmids[nzchar(pmids) & !is.na(pmids) & toupper(pmids) != "NA"]

  pmid_html <- ""
  if (length(pmids) > 0L) {
    pmid_html <- paste(
      sprintf('<span class="pmid-chip">PMID:%s</span>', .he(pmids)),
      collapse = ""
    )
  } else {
    pmid_html <- '<span class="pm-empty">No PubMed records retrieved.</span>'
  }

  sprintf(
    '<div class="pubmed-card">
      <div class="pubmed-hdr">
        <span class="pubmed-hid">%s</span>
        <span class="lit-pill lit-%s">%s literature support</span>
      </div>
      %s
      %s
      <div class="pmid-chips">%s</div>
    </div>',
    .he(.s(hid)),
    .he(support),
    .he(support),
    if (.is_nonempty(relationship))
      sprintf('<div class="pm-relationship"><strong>Phenotype relationship:</strong> %s</div>', .he(relationship))
    else "",
    if (.is_nonempty(note))
      sprintf('<div class="pm-note"><strong>Evidence note:</strong> %s</div>', .he(note))
    else "",
    pmid_html
  )
}

# Count revision actions (safe for NULL/empty)
.count_action <- function(revs, action) {
  if (is.null(revs) || length(revs) == 0L) return(0L)
  sum(vapply(revs, function(r) identical(r$action, action), logical(1L)))
}

# Wrap HTML in a chat bubble
.bubble <- function(role, avatar, label, content, extra_cls = "") {
  sprintf(
    '<div class="msg msg-%s %s">
      <div class="av av-%s" title="%s">%s</div>
      <div class="bub bub-%s">%s</div>
    </div>',
    role, extra_cls,
    role, .he(label), .he(avatar),
    role, content
  )
}

# Full inline CSS
.debate_css <- function() {
'/* ============================================================
   FeatureMAP -- module debate transcript (conversation view)
   ============================================================ */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}

:root{
  --ink:#16233a; --ink-2:#3d4c61; --muted:#7b8b9f; --faint:#9fadbe;
  --line:#e4eaf2; --line-2:#eef2f8;
  --blue:#2f6fb0;  --blue-soft:#eaf1f9;
  --teal:#28857e;  --teal-soft:#e5f3f1;
  --amber:#b56c2b; --amber-soft:#fdf3e7;
  --red:#b4443f;   --red-soft:#fceeed;
  --green:#2e8055; --green-soft:#e8f5ed;
  --slate:#5c6d84; --slate-soft:#edf1f7;

  /* speaker skins: background / border */
  --sk-query-bg:#ffffff;  --sk-query-bd:#e0e7f0;
  --sk-prop-bg:#f4f9fd;   --sk-prop-bd:#d9e6f4;
  --sk-skep-bg:#fffaf4;   --sk-skep-bd:#f1e3d2;
  --sk-pub-bg:#f7fcf9;    --sk-pub-bd:#d8e9df;
  --sk-arb-bg:#ffffff;    --sk-arb-bd:#cde4e1;

  --mono:ui-monospace,SFMono-Regular,"SF Mono",Menlo,Consolas,monospace;
  --lift:0 1px 2px rgba(18,38,66,.05), 0 8px 24px rgba(18,38,66,.06);
}

html{-webkit-text-size-adjust:100%}
body{
  font-family:"Inter",-apple-system,BlinkMacSystemFont,"Segoe UI","Helvetica Neue",Arial,sans-serif;
  background:radial-gradient(900px 380px at 50% -140px,#e8eff8 0%,rgba(232,239,248,0) 70%),#f4f6fa;
  color:var(--ink);font-size:14px;line-height:1.65;
  -webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;
}

/* optional page header (unused by the generator, kept for standalone use) */
.page-header{background:#fff;border-bottom:1px solid var(--line);padding:18px 32px 16px}
.page-header h1{font-size:1.05rem;font-weight:700;letter-spacing:-.01em;color:var(--ink)}
.page-header .sub{font-size:.76rem;color:var(--muted);margin-top:4px;display:flex;gap:14px;flex-wrap:wrap}
.page-header .sub span{display:flex;align-items:center;gap:5px}

/* -- conversation column ------------------------------------- */
.chat-body{
  max-width:920px;margin:0 auto;padding:34px 24px 60px 24px;
  display:flex;flex-direction:column;gap:20px;
}

/* -- message rows: left / right / centre --------------------- */
.msg{display:flex;gap:14px;align-items:flex-start}
.msg-left  {flex-direction:row;        justify-content:flex-start}
.msg-right {flex-direction:row-reverse;justify-content:flex-start}
.msg-center{flex-direction:column;     align-items:stretch}

/* avatars */
.av{
  flex:0 0 40px;width:40px;height:40px;border-radius:50%;margin-top:5px;
  display:flex;align-items:center;justify-content:center;font-size:17px;line-height:1;
  background:#fff;border:1px solid var(--line);
  box-shadow:0 0 0 4px var(--slate-soft),0 4px 12px rgba(18,38,66,.07);
}
.av-center{
  flex:0 0 44px;width:44px;height:44px;border-radius:50%;margin:0 auto 14px auto;
  display:flex;align-items:center;justify-content:center;font-size:19px;line-height:1;
  background:#fff;border:1px solid var(--line);
  box-shadow:0 0 0 5px var(--teal-soft),0 4px 14px rgba(18,38,66,.08);
}
.proposer-r .av{box-shadow:0 0 0 4px var(--blue-soft),0 4px 12px rgba(18,38,66,.07)}
.skeptic    .av{box-shadow:0 0 0 4px var(--amber-soft),0 4px 12px rgba(18,38,66,.07)}
.pubmed     .av{box-shadow:0 0 0 4px var(--green-soft),0 4px 12px rgba(18,38,66,.07)}

/* -- speech bubbles ------------------------------------------ */
.bub{
  position:relative;max-width:80%;min-width:0;
  background:var(--sk-query-bg);border:1px solid var(--sk-query-bd);
  border-radius:18px;padding:16px 18px 17px 18px;box-shadow:var(--lift);
}
.bub-left {border-top-left-radius:6px}
.bub-right{border-top-right-radius:6px}
.bub-center{max-width:100%;border-radius:18px;
            background:var(--sk-arb-bg);border-color:var(--sk-arb-bd);
            box-shadow:0 2px 4px rgba(18,38,66,.05),0 14px 36px rgba(18,38,66,.08)}
.msg-center .bub{max-width:100%}

/* bubble tails -- the thing that makes it read as a conversation */
.bub-left::before,.bub-right::before{
  content:"";position:absolute;top:14px;width:12px;height:12px;
  transform:rotate(45deg);background:inherit;
}
.bub-left::before {left:-7px;
  border-left:1px solid var(--sk-query-bd);border-bottom:1px solid var(--sk-query-bd);
  border-radius:0 0 0 4px}
.bub-right::before{right:-7px;
  border-right:1px solid var(--sk-query-bd);border-top:1px solid var(--sk-query-bd);
  border-radius:0 4px 0 0}

/* speaker skins */
.proposer-r .bub{background:var(--sk-prop-bg);border-color:var(--sk-prop-bd)}
.proposer-r .bub-right::before{border-color:var(--sk-prop-bd)}
.skeptic .bub{background:var(--sk-skep-bg);border-color:var(--sk-skep-bd)}
.skeptic .bub-left::before{border-color:var(--sk-skep-bd)}
.pubmed  .bub{background:var(--sk-pub-bg);border-color:var(--sk-pub-bd)}
.pubmed  .bub-left::before{border-color:var(--sk-pub-bd)}

/* -- speaker name line --------------------------------------- */
.role-label{
  display:flex;align-items:center;gap:8px;margin-bottom:11px;
  font-size:.685rem;font-weight:700;letter-spacing:.11em;text-transform:uppercase;
  color:var(--slate);
}
.msg-right .role-label{justify-content:flex-end}
.msg-center .role-label{justify-content:center;padding-bottom:12px;
                        margin-bottom:14px;border-bottom:1px solid var(--line-2)}
.proposer-r .role-label{color:var(--blue)}
.skeptic    .role-label{color:var(--amber)}
.arbiter    .role-label{color:var(--teal)}
.pubmed     .role-label{color:var(--green)}

/* what the speaker says */
.summary-line{font-size:.895rem;color:var(--ink-2);margin-bottom:13px}
.summary-line strong{font-weight:700;color:var(--ink)}
.no-challenge-note{
  font-size:.86rem;color:#256b45;font-style:normal;background:var(--green-soft);
  border:1px solid #d1e7db;border-radius:11px;padding:11px 14px;
}

/* -- query card ---------------------------------------------- */
.query-grid{display:grid;grid-template-columns:1fr 1fr;gap:12px;margin-top:10px}
.query-section{background:#f7f9fc;border:1px solid var(--line-2);
               border-radius:12px;padding:12px 14px}
.section-label{font-size:.625rem;font-weight:700;letter-spacing:.13em;
               text-transform:uppercase;color:var(--faint);margin-bottom:9px}
.met-chips{display:flex;flex-wrap:wrap;gap:6px}
.met-chip{display:inline-block;font-size:.765rem;line-height:1.55;padding:3px 11px;
          background:#fff;color:#2b4a6b;border:1px solid #dce6f2;border-radius:999px;
          white-space:normal;transition:border-color .15s,color .15s}
.met-chip:hover{border-color:#b9d1e9;color:#1d3d5c}
.muted{color:var(--faint);font-size:.82rem}

.cls-block{display:flex;flex-direction:column;gap:8px}
.cls-row{display:grid;grid-template-columns:1fr 92px 40px;align-items:center;
         gap:12px;font-size:.78rem}
.cls-name{color:var(--ink-2);line-height:1.4}
.cls-track{height:7px;background:#e7ecf4;border-radius:999px;overflow:hidden}
.cls-fill{height:100%;border-radius:999px;
          background:linear-gradient(90deg,#84aeda 0%,#3d78b4 100%)}
.cls-pct{color:var(--muted);text-align:right;white-space:nowrap;
         font-variant-numeric:tabular-nums}

.context-pills{display:flex;flex-wrap:wrap;gap:6px}
.ctx-pill{font-size:.755rem;padding:3px 11px;background:#fff;color:var(--slate);
          border:1px solid #dfe6ef;border-radius:999px}

/* -- hypothesis cards ---------------------------------------- */
.hyp-card{background:#fff;border:1px solid var(--line);border-radius:13px;
          padding:14px 16px;margin-top:11px;transition:box-shadow .18s ease}
.hyp-card:hover{box-shadow:0 6px 18px rgba(18,38,66,.075)}
.hyp-header{display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-bottom:7px}
.hyp-id{font-family:var(--mono);font-size:.655rem;font-weight:700;letter-spacing:.06em;
        color:var(--blue);background:var(--blue-soft);border:1px solid #d8e6f4;
        padding:3px 8px;border-radius:6px;flex-shrink:0}
.hyp-title{font-weight:700;font-size:.915rem;color:var(--ink);
           flex:1 1 auto;min-width:0;line-height:1.45}
.type-pill{font-size:.655rem;font-weight:600;letter-spacing:.04em;color:var(--teal);
           background:var(--teal-soft);border:1px solid #cde5e2;
           padding:3px 9px;border-radius:999px;white-space:nowrap}
.hyp-summary{font-size:.845rem;color:var(--ink-2);line-height:1.7}

.mtag-row{margin-top:10px;display:flex;flex-wrap:wrap;gap:5px;align-items:center}
.mtag-label{font-size:.615rem;font-weight:700;letter-spacing:.11em;text-transform:uppercase;
            color:var(--faint);margin-right:4px;flex-shrink:0}
.mtag{font-size:.735rem;padding:2px 9px;border-radius:999px;border:1px solid transparent}
.mtag-anc{background:var(--blue-soft);color:#255d95;border-color:#d8e6f4}
.mtag-ten{background:var(--amber-soft);color:#8d5520;border-color:#f0dcc4}

/* -- challenges ---------------------------------------------- */
.ch-item{border-radius:12px;padding:13px 15px;margin-top:10px;border:1px solid transparent}
.ch-major{background:#fdeeed;border-color:#f2d6d3}
.ch-minor{background:#fdf3e6;border-color:#f0dfc6}
.ch-meta{display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-bottom:8px}
.sev-pill{font-size:.6rem;font-weight:800;letter-spacing:.11em;
          padding:3px 10px;border-radius:999px;text-transform:uppercase}
.sev-major{background:var(--red);color:#fff}
.sev-minor{background:var(--amber);color:#fff}
.ch-type{font-size:.725rem;color:var(--muted);letter-spacing:.02em}
.ch-target{font-family:var(--mono);font-size:.71rem;font-weight:600;color:var(--ink-2);
           background:rgba(255,255,255,.85);border:1px solid rgba(22,35,58,.09);
           padding:2px 8px;border-radius:6px}
.ch-text{font-size:.845rem;color:var(--ink-2);line-height:1.7}

/* -- revisions ----------------------------------------------- */
.rev-item{padding:13px 15px;margin-top:10px;border-radius:12px;
          background:rgba(255,255,255,.72);border:1px solid rgba(22,35,58,.07)}
.rev-header{display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-bottom:7px}
.rev-hid{font-family:var(--mono);font-size:.75rem;font-weight:700;color:var(--ink)}
.act-pill{font-size:.6rem;font-weight:800;letter-spacing:.11em;
          padding:3px 10px;border-radius:999px}
.act-pill.act-modify{background:var(--blue);color:#fff}
.act-pill.act-drop  {background:var(--red);color:#fff}
.act-pill.act-keep  {background:var(--green);color:#fff}
.rev-just{font-size:.845rem;color:var(--ink-2);line-height:1.7}
.rev-detail{margin-top:11px;padding:13px 15px;background:#fff;
            border:1px solid var(--line);border-radius:11px}
.rev-new-type{display:inline-block;font-size:.655rem;font-weight:600;color:var(--teal);
              background:var(--teal-soft);border:1px solid #cde5e2;
              padding:3px 9px;border-radius:999px;margin-bottom:7px}
.rev-new-title{font-size:.875rem;font-weight:700;color:var(--ink);margin-bottom:5px}
.rev-new-summary{font-size:.82rem;color:var(--ink-2);line-height:1.65}

/* -- verdict cards ------------------------------------------- */
.verdict-card{border-radius:14px;padding:17px 19px;margin-top:12px;
              border:1px solid var(--line);background:#fff}
.st-accepted{background:linear-gradient(180deg,#f4fbf7 0%,#ffffff 60%);border-color:#cde6d8}
.st-rejected{background:linear-gradient(180deg,#fdf6f5 0%,#ffffff 60%);border-color:#eed7d4}
.verdict-hdr{display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-bottom:11px}
.verdict-hid{font-family:var(--mono);font-size:.75rem;font-weight:700;color:var(--ink);
             background:rgba(22,35,58,.05);border:1px solid rgba(22,35,58,.08);
             padding:3px 9px;border-radius:6px}
.status-pill{font-size:.6rem;font-weight:800;letter-spacing:.11em;
             padding:3px 11px;border-radius:999px}
.st-accepted .status-pill{background:var(--green);color:#fff}
.st-rejected .status-pill{background:var(--red);color:#fff}
.verdict-type{display:inline-block;font-size:.655rem;font-weight:600;color:var(--teal);
              background:var(--teal-soft);border:1px solid #cde5e2;
              padding:3px 9px;border-radius:999px;margin-bottom:7px}
.verdict-title{font-size:.945rem;font-weight:700;color:var(--ink);
               margin-bottom:6px;line-height:1.45}
.verdict-fin-summary{font-size:.845rem;color:var(--ink-2);line-height:1.7;
                     margin-bottom:13px;padding-bottom:13px;
                     border-bottom:1px solid var(--line-2)}

.rat-block{margin-top:12px}
.rat-label{font-size:.625rem;font-weight:700;letter-spacing:.13em;text-transform:uppercase;
           color:var(--faint);margin-bottom:6px;display:flex;align-items:center;gap:9px}
.rat-text{font-size:.845rem;color:var(--ink-2);line-height:1.72}
.rat-divider{height:1px;background:var(--line-2);margin:14px 0}
.conf-pill{font-size:.6rem;font-weight:700;letter-spacing:.06em;
           padding:2px 9px;border-radius:999px;text-transform:uppercase}
.conf-high  {background:var(--green-soft);color:#256b45}
.conf-medium{background:var(--amber-soft);color:#8d5520}
.conf-low   {background:var(--red-soft);color:#963a36}

.ambiguity-note{margin-top:16px;padding:13px 15px;background:var(--amber-soft);
                border:1px solid #f0dfc6;border-radius:12px;
                font-size:.845rem;line-height:1.7;color:#7d5423}
.ambiguity-note strong{color:#5f3d17;font-weight:700}

/* -- PubMed evidence ----------------------------------------- */
.pubmed-card{background:#fff;border:1px solid var(--line);border-radius:13px;
             padding:14px 16px;margin-top:11px}
.pubmed-hdr{display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-bottom:8px}
.pubmed-hid{font-family:var(--mono);font-size:.73rem;font-weight:700;color:#256b45;
            background:var(--green-soft);border:1px solid #d1e7db;
            padding:3px 9px;border-radius:6px}
.lit-pill{font-size:.6rem;font-weight:800;letter-spacing:.1em;
          padding:3px 10px;border-radius:999px;text-transform:uppercase}
.lit-direct      {background:var(--green);color:#fff}
.lit-indirect    {background:var(--green-soft);color:#1f6642;border:1px solid #cbe2d4}
.lit-insufficient{background:#eef1f5;color:var(--muted);border:1px solid #e2e8f0}
.pm-relationship,.pm-note{font-size:.845rem;color:var(--ink-2);line-height:1.7;margin-top:6px}
.pm-relationship strong,.pm-note strong{color:var(--ink);font-weight:700}
.pmid-chips{display:flex;flex-wrap:wrap;gap:6px;margin-top:11px}
.pmid-chip{font-family:var(--mono);font-size:.715rem;font-weight:600;padding:3px 10px;
           background:#f4f9f6;color:#2b6b4a;border:1px solid #d9e9e0;border-radius:999px}
.pm-empty{font-size:.8rem;color:var(--faint)}

/* -- round divider ------------------------------------------- */
.round-divider{display:flex;align-items:center;gap:16px;margin:6px 0;
               font-size:.615rem;font-weight:800;letter-spacing:.18em;
               text-transform:uppercase;color:var(--faint)}
.round-divider::before,.round-divider::after{
  content:"";flex:1;height:1px;
  background:linear-gradient(90deg,rgba(219,227,236,0),#dce4ee,rgba(219,227,236,0))}

/* -- responsive ---------------------------------------------- */
@media (max-width:760px){
  .chat-body{padding:22px 14px 44px 14px;gap:16px}
  .msg{gap:10px}
  .av{flex-basis:32px;width:32px;height:32px;font-size:14px;margin-top:4px}
  .av-center{width:38px;height:38px;font-size:16px}
  .bub{max-width:88%;padding:14px 15px 15px 15px;border-radius:16px}
  .query-grid{grid-template-columns:1fr}
  .cls-row{grid-template-columns:1fr 62px 36px;gap:9px}
}

/* -- print / PDF export -------------------------------------- */
@media print{
  body{background:#fff}
  .chat-body{max-width:none;padding:0 8px}
  .bub{max-width:100%;box-shadow:none;break-inside:avoid}
  .bub-left::before,.bub-right::before{display:none}
  .msg-right{flex-direction:row}
  .hyp-card,.verdict-card,.ch-item,.rev-item,.pubmed-card{break-inside:avoid}
}
'
}


# -- Public function -------------------------------------------

#' Export saved debate HTML files from a featuremap result
#'
#' Writes the same chat-style HTML files produced during
#' \code{annotate_module()} when \code{debate_html = TRUE}, using the saved
#' advanced-mode HTML payload in \code{res}.
#'
#' @param res A \code{featuremap_result} object containing
#'   \code{module_evidence$debate_html_payload}.
#' @param module_id Optional module id or vector of module ids to export. If
#'   \code{NULL}, all modules with saved debate HTML payloads are exported.
#' @param html_dir Directory for the output HTML files. Defaults to \code{getwd()}.
#' @return Invisibly, a named character vector of written HTML paths.
#' @export
export_debate_html <- function(res,
                               module_id = NULL,
                               html_dir = getwd()) {
  payloads <- res$module_evidence$debate_html_payload
  if (is.null(payloads) || !is.data.frame(payloads) || nrow(payloads) == 0L) {
    stop("res$module_evidence$debate_html_payload is missing. Re-run annotate_module() in advanced mode.")
  }

  if (!all(c("module_id", "payload") %in% colnames(payloads))) {
    stop("res$module_evidence$debate_html_payload must contain module_id and payload columns.")
  }

  saved_ids <- unique(as.character(payloads$module_id))
  saved_ids <- saved_ids[!is.na(saved_ids) & nzchar(saved_ids)]
  if (length(saved_ids) == 0L) {
    stop("No module ids found in res$module_evidence$debate_html_payload.")
  }

  ids <- if (is.null(module_id)) saved_ids else as.character(module_id)
  missing_ids <- setdiff(ids, saved_ids)
  if (length(missing_ids) > 0L) {
    stop("No saved debate HTML payload found for module id(s): ",
         paste(missing_ids, collapse = ", "))
  }

  paths <- vapply(ids, function(mid) {
    idx <- match(mid, as.character(payloads$module_id))
    payload <- payloads$payload[[idx]]
    do.call(generate_debate_html, c(payload, list(output_dir = html_dir)))
  }, character(1L))
  names(paths) <- ids
  invisible(paths)
}

#' Generate a chat-style debate HTML for one module
#'
#' Called automatically at the end of annotate_one_module_advanced().
#' Writes debate_<module_id>.html to output_dir (default: getwd()).
#'
#' @param module_id       Module identifier string
#' @param module_obj      Original module object (topN_metabolites used)
#' @param study_context   List with species, biospecimen, phenotype
#' @param hypotheses_v0   P0 hypotheses (list of hypothesis objects)
#' @param s1_challenges   S1 parsed challenges (list)
#' @param p1_revisions    P1 revision decisions (list)
#' @param hypotheses_v1   Hypotheses after P1 revisions (list)
#' @param s2_challenges   S2 challenges, or NULL if Round 2 did not run
#' @param p2_revisions    P2 revision decisions, or NULL
#' @param hypotheses_final Final hypotheses after all revisions
#' @param arbiter_result  List with $verdicts and $residual_ambiguity
#' @param phenotype_relationships Named list of PubMed phenotype summaries
#' @param pubmed_per_hyp Named list of retrieved PubMed records per hypothesis
#' @param output_dir      Directory for the output file (default: getwd())
#' @return Invisibly, the path of the written HTML file
generate_debate_html <- function(module_id,
                                  module_obj,
                                  study_context,
                                  hypotheses_v0,
                                  s1_challenges,
                                  p1_revisions,
                                  hypotheses_v1,
                                  s2_challenges          = NULL,
                                  p2_revisions           = NULL,
                                  hypotheses_final       = list(),
                                  arbiter_result         = list(verdicts = list(),
                                                                residual_ambiguity = NA),
                                  phenotype_relationships = NULL,
                                  pubmed_per_hyp         = NULL,
                                  output_dir             = getwd()) {

  # -- Extract module metadata ---------------------------------
  top_names <- character(0)
  if (!is.null(module_obj$topN_metabolites) &&
      "Compound_name" %in% colnames(module_obj$topN_metabolites))
    top_names <- unique(as.character(module_obj$topN_metabolites$Compound_name))

  phenotype   <- .s(study_context$phenotype,   "-")
  species     <- .s(study_context$species,     "-")
  biospecimen <- .s(study_context$biospecimen, "-")
  # -- 1. Query bubble -----------------------------------------
  ctx_pills <- paste(
    sprintf('<span class="ctx-pill">%s</span>',
            .he(c(species, biospecimen, phenotype))),
    collapse = "")

  user_content <- sprintf(
    '<div class="role-label">\U0001f50d Query</div>
    <div class="summary-line">
      <strong>%s</strong> is a detected dysregulated functional module.
      Please interpret its biological significance.
    </div>
    <div class="query-section">
      <div class="section-label">Core metabolites</div>
      <div class="met-chips">%s</div>
    </div>
    <div class="query-section" style="margin-top:8px">
      <div class="section-label">Dominant chemical classes</div>
      %s
    </div>
    <div class="query-section" style="margin-top:8px">
      <div class="section-label">Study context</div>
      <div class="context-pills">%s</div>
    </div>',
    .he(module_id),
    .met_chips(top_names),
    .class_bars(module_obj, 3L),
    ctx_pills
  )
  bubbles <- .bubble("left", "\U0001f50d", "Query", user_content)

  # -- 2. P0 Proposer ------------------------------------------
  p0_cards <- paste(vapply(hypotheses_v0, .hyp_card, character(1L)),
                    collapse = "\n")
  p0_content <- sprintf(
    '<div class="role-label proposer-r">\U0001f52c Proposer &mdash; Initial</div>
    <div class="summary-line">
      I propose <strong>%d</strong> candidate hypothesis%s based on the module data:
    </div>
    %s',
    length(hypotheses_v0),
    if (length(hypotheses_v0) != 1L) "es" else "",
    p0_cards
  )
  bubbles <- paste0(bubbles, "\n",
                    .bubble("right", "\U0001f52c", "Proposer", p0_content, "proposer-r"))

  # -- 3. S1 Skeptic -------------------------------------------
  real_s1 <- Filter(function(c) !identical(c$severity, "no_challenge"), s1_challenges)
  if (length(real_s1) > 0L) {
    s1_items  <- paste(vapply(real_s1, .ch_item, character(1L)), collapse = "\n")
    s1_content <- sprintf(
      '<div class="role-label">\U0001f9d0 Skeptic &mdash; Round 1</div>
      <div class="summary-line">I raise <strong>%d</strong> challenge%s:</div>
      %s',
      length(real_s1),
      if (length(real_s1) != 1L) "s" else "",
      s1_items
    )
  } else {
    s1_content <- sprintf(
      '<div class="role-label">\U0001f9d0 Skeptic &mdash; Round 1</div>
      <div class="no-challenge-note">%s</div>',
      "No substantive challenges. The proposed hypotheses are well-supported by the data."
    )
  }
  bubbles <- paste0(bubbles, "\n",
                    .bubble("left", "\U0001f9d0", "Skeptic", s1_content, "skeptic"))

  # -- 4. P1 Proposer revision ---------------------------------
  p1_items    <- paste(vapply(p1_revisions, .rev_item,
                              character(1L), hypotheses_after = hypotheses_v1),
                       collapse = "\n")
  modified_r1 <- .count_action(p1_revisions, "modify")
  dropped_r1  <- .count_action(p1_revisions, "drop")
  kept_r1     <- length(p1_revisions) - modified_r1 - dropped_r1
  p1_content  <- sprintf(
    '<div class="role-label proposer-r">&#9999;&#65039; Proposer &mdash; Revision 1</div>
    <div class="summary-line">
      Addressing Round 1:
      <strong>%d modified</strong>, <strong>%d dropped</strong>, %d kept.
    </div>
    %s',
    modified_r1, dropped_r1, kept_r1, p1_items
  )
  bubbles <- paste0(bubbles, "\n",
                    .bubble("right", "&#9999;&#65039;", "Proposer", p1_content, "proposer-r"))

  # -- 5. S2 Skeptic (conditional) -----------------------------
  if (!is.null(s2_challenges) && length(s2_challenges) > 0L) {
    real_s2 <- Filter(function(c) !identical(c$severity, "no_challenge"), s2_challenges)
    bubbles <- paste0(bubbles,
                      '\n<div class="round-divider">Round 2</div>')
    if (length(real_s2) > 0L) {
      s2_items   <- paste(vapply(real_s2, .ch_item, character(1L)), collapse = "\n")
      s2_content <- sprintf(
        '<div class="role-label">\U0001f9d0 Skeptic &mdash; Round 2</div>
        <div class="summary-line">After revisions, I raise <strong>%d</strong> further challenge%s:</div>
        %s',
        length(real_s2),
        if (length(real_s2) != 1L) "s" else "",
        s2_items
      )
      bubbles <- paste0(bubbles, "\n",
                        .bubble("left", "\U0001f9d0", "Skeptic", s2_content, "skeptic"))
    } else {
      s2_content <- sprintf(
        '<div class="role-label">\U0001f9d0 Skeptic &mdash; Round 2</div>
        <div class="no-challenge-note">%s</div>',
        "No substantive challenges after reviewing the revision."
      )
      bubbles <- paste0(bubbles, "\n",
                        .bubble("left", "\U0001f9d0", "Skeptic", s2_content, "skeptic"))
    }
  }

  # -- 6. P2 Proposer revision (conditional) -------------------
  if (!is.null(p2_revisions) && length(p2_revisions) > 0L) {
    p2_items    <- paste(vapply(p2_revisions, .rev_item,
                                character(1L), hypotheses_after = hypotheses_final),
                         collapse = "\n")
    modified_r2 <- .count_action(p2_revisions, "modify")
    dropped_r2  <- .count_action(p2_revisions, "drop")
    kept_r2     <- length(p2_revisions) - modified_r2 - dropped_r2
    p2_content  <- sprintf(
      '<div class="role-label proposer-r">&#9999;&#65039; Proposer &mdash; Revision 2</div>
      <div class="summary-line">
        Addressing Round 2:
        <strong>%d modified</strong>, <strong>%d dropped</strong>, %d kept.
      </div>
      %s',
      modified_r2, dropped_r2, kept_r2, p2_items
    )
    bubbles <- paste0(bubbles, "\n",
                      .bubble("right", "&#9999;&#65039;", "Proposer", p2_content, "proposer-r"))
  }

  # -- 7. PubMed phenotype evidence ----------------------------
  pubmed_ids <- unique(c(names(phenotype_relationships %||% list()),
                         names(pubmed_per_hyp %||% list())))
  pubmed_ids <- pubmed_ids[nzchar(pubmed_ids)]
  if (length(pubmed_ids) > 0L) {
    pm_cards <- paste(vapply(pubmed_ids, function(hid) {
      .pubmed_card(
        hid = hid,
        rel = (phenotype_relationships %||% list())[[hid]],
        evidence = (pubmed_per_hyp %||% list())[[hid]]
      )
    }, character(1L)), collapse = "\n")
    pm_content <- sprintf(
      '<div class="role-label">&#128218; PubMed phenotype evidence</div>
      <div class="summary-line">
        Retrieved literature and phenotype relationship summaries used by the Arbiter:
      </div>
      %s',
      pm_cards
    )
    bubbles <- paste0(bubbles, "\n",
                      .bubble("left", "&#128218;", "PubMed", pm_content, "pubmed"))
  }

  # -- 8. A0 Arbiter -------------------------------------------
  verdicts <- arbiter_result$verdicts %||% list()
  n_acc    <- sum(vapply(verdicts, function(v) identical(v$status, "accepted"),  logical(1L)))
  n_rej    <- sum(vapply(verdicts, function(v) identical(v$status, "rejected"),  logical(1L)))

  vcards <- paste(vapply(verdicts, .verdict_card,
                         character(1L), hypotheses_final = hypotheses_final),
                  collapse = "\n")

  amb      <- .s(arbiter_result$residual_ambiguity, "")
  amb_html <- if (.is_nonempty(amb))
    sprintf('<div class="ambiguity-note"><strong>Residual ambiguity:</strong> %s</div>',
            .he(amb))
  else ""

  arbiter_content <- sprintf(
    '<div class="role-label">&#9878; Arbiter &mdash; Final Verdict</div>
    <div class="summary-line">
      Reviewing the full debate:
      <strong>%d accepted</strong> &middot;
      <strong>%d rejected</strong>.
    </div>
    %s%s',
    n_acc, n_rej, vcards, amb_html
  )
  bubbles <- paste0(bubbles, "\n",
                    .bubble("center", "&#9878;", "Arbiter",
                            arbiter_content, "arbiter"))

  # -- Assemble page --------------------------------------------
  html <- sprintf(
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Module Debate &mdash; %s</title>
<style>%s</style>
</head>
<body>
<div class="chat-body">
%s
</div>
</body>
</html>',
    .he(module_id),
    .debate_css(),
    bubbles
  )

  fname    <- gsub("[^A-Za-z0-9._-]", "_", module_id)
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, sprintf("debate_%s.html", fname))
  writeLines(html, out_path, useBytes = FALSE)
  message(sprintf("[HTML] Debate visualization -> %s", out_path))
  invisible(out_path)
}

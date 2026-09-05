# ============================================================
# 47_synthesis_html.R
# Interactive hypothesis-level synthesis network HTML
# ============================================================

.syn_he <- function(s) {
  s <- as.character(s %||% "")
  s <- gsub("&",  "&amp;",  s, fixed = TRUE)
  s <- gsub("<",  "&lt;",   s, fixed = TRUE)
  s <- gsub(">",  "&gt;",   s, fixed = TRUE)
  s <- gsub('"',  "&quot;", s, fixed = TRUE)
  s
}

.syn_scalar <- function(x, default = "") {
  x <- x %||% default
  x <- as.character(x)
  if (length(x) == 0L) return(as.character(default)[1L])
  x <- trimws(x[1L])
  if (is.na(x) || !nzchar(x) || toupper(x) %in% c("NA", "NULL")) {
    return(as.character(default)[1L])
  }
  x
}

.syn_get_synthesis <- function(x) {
  if (!is.null(x$module_synthesis)) return(x$module_synthesis)
  x
}

.syn_json_records <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(list())
  unname(lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    lapply(row, function(x) .syn_scalar(x, ""))
  }))
}

# ── CSS ───────────────────────────────────────────────────────
.syn_css <- function() {
'/* ============================================================
   FeatureMAP -- cross-module synthesis network
   ============================================================ */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}

:root{
  --ink:#16233a; --ink-2:#3d4c61; --muted:#7b8b9f; --faint:#9fadbe;
  --line:#e4eaf2; --line-2:#eef2f8;
  --blue:#2f6fb0;  --blue-soft:#eaf1f9;
  --teal:#28857e;  --teal-soft:#e5f3f1;
  --amber:#b56c2b; --amber-soft:#fdf3e7;
  --green:#2e8055; --green-soft:#e8f5ed;
  --violet:#6f5ea8;--violet-soft:#f0edf8;
  --slate:#5c6d84; --slate-soft:#edf1f7;
  --canvas:#fbfcfe;
  --mono:ui-monospace,SFMono-Regular,"SF Mono",Menlo,Consolas,monospace;
  --lift:0 1px 2px rgba(18,38,66,.05), 0 8px 24px rgba(18,38,66,.06);
}

html{-webkit-text-size-adjust:100%}
body{
  font-family:"Inter",-apple-system,BlinkMacSystemFont,"Segoe UI","Helvetica Neue",Arial,sans-serif;
  background:radial-gradient(900px 380px at 50% -140px,#e8eff8 0%,rgba(232,239,248,0) 70%),#f4f6fa;
  color:var(--ink);font-size:14px;line-height:1.6;
  -webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;
}

/* optional page header (unused by the generator, kept for standalone use) */
.page-header{background:#fff;border-bottom:1px solid var(--line);padding:18px 30px 16px}
.page-header h1{font-size:1.05rem;font-weight:700;letter-spacing:-.01em;color:var(--ink)}
.page-header .sub{font-size:.78rem;color:var(--muted);margin-top:4px}
.ctx-pill{display:inline-flex;align-items:center;font-size:.75rem;padding:3px 11px;
          border-radius:999px;background:#fff;border:1px solid #dfe6ef;
          color:var(--slate);margin-right:6px}

/* ── layout ────────────────────────────────────────────────── */
.layout{display:grid;grid-template-columns:minmax(0,1fr) 324px;gap:18px;
        max-width:1400px;margin:0 auto;padding:24px 22px 0 22px}
.panel{background:#fff;border:1px solid var(--line);border-radius:16px;
       overflow:hidden;box-shadow:var(--lift)}

/* ── toolbar ───────────────────────────────────────────────── */
.toolbar{padding:13px 16px;display:flex;align-items:center;gap:10px;flex-wrap:wrap;
         border-bottom:1px solid var(--line-2);background:#fff}
.toolbar label{
  font-size:.775rem;font-weight:600;color:var(--slate);cursor:pointer;
  display:inline-flex;align-items:center;gap:7px;
  padding:5px 13px 5px 10px;border:1px solid var(--line);border-radius:999px;
  background:#fff;transition:border-color .15s,background .15s,color .15s;
}
.toolbar label:hover{border-color:#c9d8e8}
.toolbar label:has(input:checked){background:var(--blue-soft);border-color:#d3e3f3;color:#25588c}
.toolbar input[type=checkbox]{accent-color:var(--blue);width:14px;height:14px;cursor:pointer}
.toolbar input[type=text]{
  margin-left:auto;border:1px solid var(--line);border-radius:999px;
  padding:7px 15px;min-width:250px;font-size:.815rem;color:var(--ink);
  background:#f9fbfd;outline:none;transition:border-color .15s,background .15s,box-shadow .15s;
}
.toolbar input[type=text]::placeholder{color:var(--faint)}
.toolbar input[type=text]:focus{border-color:#a9c8e6;background:#fff;
                                box-shadow:0 0 0 3px rgba(47,111,176,.10)}

/* ── legend ────────────────────────────────────────────────── */
.legend{padding:10px 16px;display:flex;gap:14px;flex-wrap:wrap;align-items:center;
        color:var(--muted);font-size:.735rem;font-weight:500;
        background:#f9fbfd;border-bottom:1px solid var(--line-2)}
.legend span{display:inline-flex;align-items:center;gap:5px}
.leg-circle{width:11px;height:11px;border-radius:50%;background:var(--slate);flex-shrink:0}
.leg-edge-d{width:20px;height:2px;border-radius:2px;background:var(--blue);flex-shrink:0}
.leg-edge-i{width:20px;height:0;border-top:2px dashed var(--amber);flex-shrink:0}
.leg-sep{width:1px;height:12px;background:#dbe3ee}
#module-legend{display:flex;gap:9px;flex-wrap:wrap}
#module-legend span{font-size:.725rem;color:var(--ink-2);
                    background:#fff;border:1px solid var(--line);
                    padding:2px 9px 2px 7px;border-radius:999px}
.mod-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0}

/* ── network canvas ────────────────────────────────────────── */
.network-wrap{position:relative;overflow:hidden;height:620px}
svg{width:100%;height:100%;display:block;
    background-color:var(--canvas);
    background-image:radial-gradient(#e3eaf3 1px,transparent 1px);
    background-size:22px 22px;background-position:-1px -1px}
.edge{stroke-opacity:.62;transition:stroke-opacity .15s}
.edge.direct  {stroke:var(--blue);stroke-width:2;stroke-linecap:round}
.edge.indirect{stroke:var(--amber);stroke-width:1.7;stroke-dasharray:5 4;stroke-linecap:round}
.node{cursor:pointer}
.node text{font-size:10.5px;font-weight:600;fill:var(--ink);paint-order:stroke;
           stroke:var(--canvas);stroke-width:3px;user-select:none;pointer-events:none}

/* hover tooltip */
#tooltip{position:absolute;pointer-events:none;display:none;z-index:30;
         background:rgba(19,31,50,.90);color:#fff;
         padding:6px 11px;border-radius:8px;
         box-shadow:0 6px 18px rgba(12,24,44,.24);
         font-size:.76rem;font-weight:500;white-space:nowrap;
         max-width:280px;overflow:hidden;text-overflow:ellipsis;line-height:1.45}

/* pinned card */
#pinned{position:absolute;display:none;z-index:40;
        background:#fff;border:1px solid var(--line);border-radius:14px;
        box-shadow:0 2px 6px rgba(18,38,66,.06),0 18px 44px rgba(18,38,66,.16);
        width:292px;max-height:300px;flex-direction:column;overflow:hidden}
#pinned .pin-hdr{padding:13px 14px 11px 14px;border-bottom:1px solid var(--line-2);
                 display:flex;align-items:flex-start;gap:8px;flex-shrink:0;
                 background:#fbfcfe}
#pinned .pin-id{font-family:var(--mono);font-size:.66rem;font-weight:700;letter-spacing:.05em;
                color:var(--blue);background:var(--blue-soft);border:1px solid #d8e6f4;
                padding:3px 8px;border-radius:6px;flex-shrink:0}
#pinned .pin-pills{display:flex;flex-wrap:wrap;gap:4px;margin-top:6px}
#pinned .pin-pill{font-size:.655rem;font-weight:600;padding:2px 8px;border-radius:999px;
                  background:var(--slate-soft);color:var(--slate);border:1px solid #dfe6ef}
#pinned .pin-pill.accepted{background:var(--green-soft);color:#256b45;border-color:#d1e7db}
#pinned .pin-close{margin-left:auto;flex-shrink:0;cursor:pointer;font-size:.95rem;
                   color:var(--faint);line-height:1;padding:2px 3px;user-select:none;
                   border-radius:5px;transition:color .15s,background .15s}
#pinned .pin-close:hover{color:var(--ink);background:var(--slate-soft)}
#pinned .pin-body{padding:12px 14px 14px 14px;overflow-y:auto;flex:1}
#pinned .pin-title{font-size:.855rem;font-weight:700;color:var(--ink);
                   line-height:1.5;margin-bottom:9px}
#pinned .pin-section{margin-top:11px;padding-top:11px;border-top:1px solid var(--line-2)}
#pinned .pin-section:first-child{margin-top:0;padding-top:0;border-top:none}
#pinned .pin-lbl{font-size:.615rem;font-weight:700;letter-spacing:.12em;
                 text-transform:uppercase;color:var(--faint);margin-bottom:4px}
#pinned .pin-txt{font-size:.795rem;color:var(--ink-2);line-height:1.68;white-space:pre-wrap}

/* ── integrated summary panel ──────────────────────────────── */
.sum-panel{padding:18px 19px;display:flex;flex-direction:column;
           background:linear-gradient(180deg,#fbfdff 0%,#ffffff 120px)}
.sum-panel h2{font-size:.9rem;font-weight:700;margin-bottom:8px;color:var(--ink)}
.sum-label{font-size:.615rem;font-weight:700;letter-spacing:.13em;text-transform:uppercase;
           color:var(--faint);margin-bottom:10px;padding-bottom:11px;
           border-bottom:1px solid var(--line-2)}
.sum-text{font-size:.825rem;color:var(--ink-2);line-height:1.75;white-space:pre-wrap;
          overflow-y:auto;max-height:640px}
.sum-empty{font-size:.82rem;color:var(--faint)}

/* ── bottom tables ─────────────────────────────────────────── */
.table-wrap{max-width:1400px;margin:20px auto 30px auto;padding:0 22px}
.tab-bar{display:flex;gap:4px;border-bottom:1px solid var(--line);margin-bottom:0}
.tab-btn{border:none;background:none;padding:10px 4px;margin-right:22px;
         font-size:.815rem;font-weight:600;color:var(--muted);cursor:pointer;
         border-bottom:2px solid transparent;margin-bottom:-1px;transition:color .15s}
.tab-btn.active{color:var(--blue);border-bottom-color:var(--blue)}
.tab-btn:hover:not(.active){color:var(--ink-2)}
.tab-panel{display:none}
.tab-panel.active{display:block}

.node-tbl{width:100%;border-collapse:separate;border-spacing:0;background:#fff;
          border:1px solid var(--line);border-top:none;
          border-radius:0 0 16px 16px;overflow:hidden;
          font-size:.79rem;box-shadow:var(--lift)}
.node-tbl thead tr{background:#f9fbfd}
.node-tbl th{padding:11px 14px;text-align:left;font-size:.625rem;font-weight:700;
             letter-spacing:.12em;text-transform:uppercase;color:var(--faint);
             border-bottom:1px solid var(--line)}
.node-tbl td{padding:11px 14px;border-bottom:1px solid var(--line-2);
             color:var(--ink-2);vertical-align:top;line-height:1.6}
.node-tbl tr:last-child td{border-bottom:none}
.node-tbl tbody tr{transition:background .12s}
.node-tbl tbody tr:hover td{background:#f8fbfe}
.tbl-id{font-family:var(--mono);font-weight:700;color:var(--blue);white-space:nowrap}
.tbl-mod{white-space:nowrap;color:var(--ink)}
.tbl-pill{display:inline-block;font-size:.645rem;font-weight:700;letter-spacing:.06em;
          padding:3px 9px;border-radius:999px;white-space:nowrap;text-transform:uppercase}
.tbl-direct  {background:var(--blue-soft);color:#25588c;border:1px solid #d8e6f4}
.tbl-indirect{background:var(--amber-soft);color:#8d5520;border:1px solid #f0dcc4}
.tbl-conf{color:var(--muted);font-size:.755rem}
.tbl-rat{font-size:.775rem;color:var(--muted);line-height:1.65}

/* hypothesis type pills -- pinned card + table */
.type-endo {background:var(--green-soft);color:#256b45;border:1px solid #d1e7db}
.type-micro{background:var(--amber-soft);color:#8d5520;border:1px solid #f0dcc4}
.type-diet {background:var(--blue-soft);color:#25588c;border:1px solid #d8e6f4}
.type-xeno {background:var(--violet-soft);color:#584a86;border:1px solid #dfd8f0}
.type-mixed{background:var(--slate-soft);color:var(--slate);border:1px solid #dfe6ef}
.tbl-type{display:inline-block;font-size:.65rem;font-weight:600;padding:3px 9px;
          border-radius:999px;white-space:nowrap;border:1px solid #dfe6ef;
          background:var(--slate-soft);color:var(--slate)}

/* ── responsive ────────────────────────────────────────────── */
@media(max-width:980px){
  .layout{grid-template-columns:minmax(0,1fr);padding:18px 14px 0 14px;gap:14px}
  .network-wrap{height:460px}
  .sum-text{max-height:none}
  .table-wrap{padding:0 14px}
  .toolbar input[type=text]{margin-left:0;min-width:0;width:100%}
}

/* ── print / PDF export ────────────────────────────────────── */
@media print{
  body{background:#fff}
  .panel,.node-tbl{box-shadow:none}
  .toolbar,.tab-bar{display:none}
  .tab-panel{display:block !important;margin-top:16px}
  .node-tbl{border-top:1px solid var(--line);border-radius:12px}
  .node-tbl tr{break-inside:avoid}
}'
}

# ── JavaScript ────────────────────────────────────────────────
.syn_js <- function() {
'const data = GRAPH_DATA;
const svg    = document.getElementById("network");
const gEdges = document.getElementById("edges");
const gNodes = document.getElementById("nodes");
const tooltip = document.getElementById("tooltip");
const pinned  = document.getElementById("pinned");
const search  = document.getElementById("search");
const controls = {
  direct:   document.getElementById("fDirect"),
  indirect: document.getElementById("fIndirect")
};
let pinnedNode = null;

/* ── colour / size maps ── */
const PALETTE = [
  "#3d7ebb","#2f9189","#c07b3a","#3f9b6a",
  "#8a6fb8","#c2607f","#4a8fb0","#a08a34",
  "#6b8f45","#a5634a","#5f74b5","#7c8a99"
];
const moduleIds   = [...new Set(data.nodes.map(d => d.module_id))].sort();
const moduleColor = Object.fromEntries(
  moduleIds.map((m, i) => [m, PALETTE[i % PALETTE.length]])
);
const SIZE = {high: 17, medium: 13, low: 10};
const DEF_SIZE = 12;

function nodeSize(n) { return SIZE[n.confidence_label] || DEF_SIZE; }
function nodeColor(n){ return moduleColor[n.module_id] || "#5c6d84"; }

/* ── module colour legend ── */
const modLeg = document.getElementById("module-legend");
moduleIds.forEach(m => {
  const sp = document.createElement("span");
  sp.innerHTML = `<i class="mod-dot" style="background:${nodeColor({module_id:m})}"></i>${esc(m)}`;
  modLeg.appendChild(sp);
});

/* ── node / edge data ── */
let nodes = data.nodes.map(d => ({...d, x: 0, y: 0}));
const nodeById = Object.fromEntries(nodes.map(d => [d.node_id, d]));
let edges = data.edges
  .map(e => ({...e, source: nodeById[e.node_1], target: nodeById[e.node_2]}))
  .filter(e => e.source && e.target);

/* ── helpers ── */
function esc(s){
  return String(s ?? "")
    .replace(/&/g,"&amp;")
    .replace(/</g,"&lt;")
    .replace(/>/g,"&gt;")
    .replace(/"/g,"&quot;");
}

function typeCls(t){
  if (!t) return "";
  const tl = t.toLowerCase();
  if (tl.includes("host") && tl.includes("microbiome")) return "type-mixed";
  if (tl.includes("microbiome") || tl.includes("microbial"))  return "type-micro";
  if (tl.includes("diet") || tl.includes("food"))             return "type-diet";
  if (tl.includes("xenobiotic") || tl.includes("exogenous"))  return "type-xeno";
  if (tl.includes("endogenous") || tl.includes("host"))       return "type-endo";
  return "";
}

function filters(){
  return {
    direct:   controls.direct.checked,
    indirect: controls.indirect.checked,
    q: search.value.trim().toLowerCase()
  };
}

function nodeVisible(n, f){
  if (f.q){
    const hay = [n.node_id,n.module_id,n.hypothesis_id,n.hypothesis_type,
                 n.title,n.functional_summary,n.phenotype_relationship]
                .join(" ").toLowerCase();
    if (!hay.includes(f.q)) return false;
  }
  return true;
}

function edgeVisible(e, visIds, f){
  if (!visIds.has(e.node_1) || !visIds.has(e.node_2)) return false;
  if (e.relation_class === "direct")   return f.direct;
  if (e.relation_class === "indirect") return f.indirect;
  return false;
}

/* ── hit test: all nodes are circles ── */
function isOver(n, mx, my){
  const sz = nodeSize(n);
  const dx = n.x - mx, dy = n.y - my;
  return dx*dx + dy*dy <= sz*sz;
}

/* ── hit test: point-to-segment distance for edge tooltips ── */
function ptSegDist(px, py, x1, y1, x2, y2){
  const dx = x2-x1, dy = y2-y1;
  const lenSq = dx*dx + dy*dy;
  if (lenSq === 0) return Math.hypot(px-x1, py-y1);
  const t = Math.max(0, Math.min(1, ((px-x1)*dx + (py-y1)*dy) / lenSq));
  return Math.hypot(px-(x1+t*dx), py-(y1+t*dy));
}

/* ── hover tooltip — title only, follows mouse, no pointer-events ── */
function showTooltip(ev, n){
  tooltip.textContent = n.title || n.node_id;
  tooltip.style.display = "block";
  positionTooltip(ev);
}

function positionTooltip(ev){
  const wrap = document.querySelector(".network-wrap");
  const r    = wrap.getBoundingClientRect();
  let tx = ev.clientX - r.left + 14;
  let ty = ev.clientY - r.top  - 30;
  if (tx + 270 > wrap.offsetWidth) tx = ev.clientX - r.left - 270;
  if (ty < 4) ty = ev.clientY - r.top + 14;
  tooltip.style.left = Math.max(4, tx) + "px";
  tooltip.style.top  = Math.max(4, ty) + "px";
}

/* ── pinned card — click to open, scrollable ── */
function pinSection(label, text){
  if (!text || !text.trim()) return "";
  return `<div class="pin-section">
    <div class="pin-lbl">${esc(label)}</div>
    <div class="pin-txt">${esc(text)}</div>
  </div>`;
}

function showPin(ev, n){
  pinnedNode = n.node_id;
  const confLabel = n.confidence_label ? n.confidence_label + " confidence" : "";
  pinned.innerHTML =
    `<div class="pin-hdr">
       <div>
         <span class="pin-id">${esc(n.node_id)}</span>
         <div class="pin-pills">
           ${confLabel ? `<span class="pin-pill">${esc(confLabel)}</span>` : ""}
           ${n.hypothesis_type ? `<span class="pin-pill ${typeCls(n.hypothesis_type)}">${esc(n.hypothesis_type)}</span>` : ""}
         </div>
       </div>
       <span class="pin-close" id="pin-close-btn">&#x2715;</span>
     </div>
     <div class="pin-body">
       ${n.title ? `<div class="pin-title">${esc(n.title)}</div>` : ""}
       ${pinSection("Functional summary", n.functional_summary)}
     </div>`;

  /* position near node */
  const wrap = document.querySelector(".network-wrap");
  const wW = wrap.offsetWidth, wH = wrap.offsetHeight;
  const cardW = 270, cardH = Math.min(280, wH - 20);
  let px = n.x + nodeSize(n) + 10;
  let py = n.y - 60;
  if (px + cardW > wW - 8) px = n.x - nodeSize(n) - cardW - 10;
  if (py + cardH > wH - 8) py = wH - cardH - 8;
  if (py < 8) py = 8;
  if (px < 8) px = 8;

  pinned.style.left    = px + "px";
  pinned.style.top     = py + "px";
  pinned.style.display = "flex";

  /* close button */
  document.getElementById("pin-close-btn")
    .addEventListener("click", ev2 => { ev2.stopPropagation(); hidePin(); });
}

function hidePin(){
  pinnedNode = null;
  pinned.style.display = "none";
  render();
}

/* ── SVG-level mouse tracking (survives DOM re-renders every tick) ── */
svg.addEventListener("mousemove", ev => {
  const r  = svg.getBoundingClientRect();
  const mx = ev.clientX - r.left;
  const my = ev.clientY - r.top;
  const f  = filters();
  const vis    = nodes.filter(n => nodeVisible(n, f));
  const visIds = new Set(vis.map(n => n.node_id));
  let hit = null;
  for (let i = vis.length - 1; i >= 0; i--) {
    if (isOver(vis[i], mx, my)) { hit = vis[i]; break; }
  }
  if (hit && hit.node_id !== pinnedNode) {
    /* node hover: show title */
    showTooltip(ev, hit);
  } else if (!hit) {
    /* edge hover: find nearest visible edge within 6px */
    const visE = edges.filter(e => edgeVisible(e, visIds, f));
    let hitEdge = null;
    for (const e of visE) {
      if (ptSegDist(mx, my,
                    e.source.x, e.source.y,
                    e.target.x, e.target.y) < 6) {
        hitEdge = e; break;
      }
    }
    if (hitEdge) {
      tooltip.textContent = hitEdge.relation_class + "  ·  " + (hitEdge.subtype || "");
      tooltip.style.display = "block";
      positionTooltip(ev);
    } else {
      tooltip.style.display = "none";
    }
  } else {
    tooltip.style.display = "none";
  }
});

svg.addEventListener("mouseleave", () => {
  tooltip.style.display = "none";
});

/* click on SVG background dismisses pin */
svg.addEventListener("click", ev => {
  const r  = svg.getBoundingClientRect();
  const mx = ev.clientX - r.left;
  const my = ev.clientY - r.top;
  const f  = filters();
  const vis = nodes.filter(n => nodeVisible(n, f));
  const hit = vis.find(n => isOver(n, mx, my));
  if (hit) {
    tooltip.style.display = "none";
    showPin(ev, hit);
    render();  /* re-render to highlight selected node */
  } else {
    hidePin();
  }
});

/* ── static layout ── */
function computeLayout(){
  const rect = svg.getBoundingClientRect();
  const W = rect.width  || 860;
  const H = rect.height || 560;
  const cx = W / 2, cy = H / 2;

  /* group nodes by module */
  const groups = {};
  nodes.forEach(n => { (groups[n.module_id] = groups[n.module_id] || []).push(n); });
  const mids = Object.keys(groups).sort();
  const nMod = mids.length;

  /* place module centres in a circle */
  const Rmod = Math.min(cx, cy) * (nMod <= 2 ? 0.30 : nMod <= 4 ? 0.44 : 0.52);
  mids.forEach((mid, mi) => {
    const angle = 2 * Math.PI * mi / nMod - Math.PI / 2;
    const mcx   = cx + Rmod * Math.cos(angle);
    const mcy   = cy + Rmod * Math.sin(angle);
    const grp   = groups[mid];
    const nN    = grp.length;
    /* spread nodes in a small circle around module centre */
    const Rnode = nN === 1 ? 0 : Math.max(28, nN * 13);
    grp.forEach((n, ni) => {
      const a = 2 * Math.PI * ni / nN - Math.PI / 2;
      n.x = mcx + Rnode * Math.cos(a);
      n.y = mcy + Rnode * Math.sin(a);
    });
  });

  /* iterative overlap resolution — no animation, just converge */
  for (let iter = 0; iter < 200; iter++){
    /* repulsion between all pairs */
    for (let i = 0; i < nodes.length; i++){
      for (let j = i+1; j < nodes.length; j++){
        const a = nodes[i], b = nodes[j];
        let dx = a.x - b.x, dy = a.y - b.y;
        const d2 = dx*dx + dy*dy + 0.01;
        const d  = Math.sqrt(d2);
        const minD = nodeSize(a) + nodeSize(b) + 44;
        if (d < minD){
          const push = (minD - d) * 0.45;
          dx /= d; dy /= d;
          a.x += dx * push; a.y += dy * push;
          b.x -= dx * push; b.y -= dy * push;
        }
      }
    }
    /* gentle spring along edges (pull connected nodes closer) */
    edges.forEach(e => {
      const a = e.source, b = e.target;
      let dx = b.x - a.x, dy = b.y - a.y;
      const d = Math.sqrt(dx*dx + dy*dy) + 0.01;
      const target = e.relation_class === "direct" ? 140 : 180;
      if (d > target){
        const pull = (d - target) * 0.04;
        dx /= d; dy /= d;
        a.x += dx * pull; a.y += dy * pull;
        b.x -= dx * pull; b.y -= dy * pull;
      }
    });
    /* soft pull back to module centre */
    mids.forEach(mid => {
      const grp = groups[mid];
      const mcx = grp.reduce((s, n) => s + n.x, 0) / grp.length;  /* current centroid */
      const mcy = grp.reduce((s, n) => s + n.y, 0) / grp.length;
    });
    /* boundary clamp */
    nodes.forEach(n => {
      const pad = nodeSize(n) + 16;
      n.x = Math.max(pad, Math.min(W - pad, n.x));
      n.y = Math.max(pad, Math.min(H - pad, n.y));
    });
  }
}

/* ── render ── */
function render(){
  const f      = filters();
  const vis    = nodes.filter(n => nodeVisible(n, f));
  const visIds = new Set(vis.map(n => n.node_id));
  const visE   = edges.filter(e => edgeVisible(e, visIds, f));

  /* edges */
  gEdges.innerHTML = "";
  for (const e of visE){
    const line = document.createElementNS("http://www.w3.org/2000/svg","line");
    line.setAttribute("x1", e.source.x); line.setAttribute("y1", e.source.y);
    line.setAttribute("x2", e.target.x); line.setAttribute("y2", e.target.y);
    line.setAttribute("class", `edge ${e.relation_class}`);
    gEdges.appendChild(line);
  }

  /* nodes */
  gNodes.innerHTML = "";
  for (const n of vis){
    const grp = document.createElementNS("http://www.w3.org/2000/svg","g");
    grp.setAttribute("class", "node");
    grp.setAttribute("transform", `translate(${n.x},${n.y})`);

    const sz   = nodeSize(n);
    const fill = nodeColor(n);
    let shape;

    shape = document.createElementNS("http://www.w3.org/2000/svg","circle");
    shape.setAttribute("r", sz);
    const isPinned = n.node_id === pinnedNode;
    shape.setAttribute("fill", fill);
    shape.setAttribute("stroke", isPinned ? "#16233a" : "#fff");
    shape.setAttribute("stroke-width", isPinned ? "3.5" : "2.5");

    const t = document.createElementNS("http://www.w3.org/2000/svg","text");
    t.setAttribute("x", sz + 4);
    t.setAttribute("y", 4);
    t.textContent = n.node_id;

    grp.appendChild(shape);
    grp.appendChild(t);
    gNodes.appendChild(grp);
  }
}

/* ── node table ── */
const tbody = document.getElementById("tbl-body");
nodes.forEach(n => {
  const tr = document.createElement("tr");
  tr.innerHTML =
    `<td class="tbl-id">${esc(n.node_id)}</td>` +
    `<td class="tbl-mod">${esc(n.module_id)}</td>` +
    `<td><span class="tbl-type ${typeCls(n.hypothesis_type)}">${esc(n.hypothesis_type || "—")}</span></td>` +
    `<td class="tbl-conf">${esc(n.confidence_label || "—")}</td>` +
    `<td>${esc(n.title || "—")}</td>`;
  tbody.appendChild(tr);
});

/* ── edge table ── */
const etbody = document.getElementById("etbl-body");
data.edges.forEach(e => {
  const tr = document.createElement("tr");
  const pillCls = e.relation_class === "direct" ? "tbl-direct" : "tbl-indirect";
  tr.innerHTML =
    `<td class="tbl-id">${esc(e.node_1)}</td>` +
    `<td class="tbl-id">${esc(e.node_2)}</td>` +
    `<td><span class="tbl-pill ${pillCls}">${esc(e.relation_class)}</span></td>` +
    `<td>${esc(e.subtype || "—")}</td>` +
    `<td class="tbl-rat">${esc(e.rationale || "—")}</td>`;
  etbody.appendChild(tr);
});

/* ── tab switching ── */
document.querySelectorAll(".tab-btn").forEach(btn => {
  btn.addEventListener("click", () => {
    document.querySelectorAll(".tab-btn").forEach(b => b.classList.remove("active"));
    document.querySelectorAll(".tab-panel").forEach(p => p.classList.remove("active"));
    btn.classList.add("active");
    document.getElementById("tab-" + btn.dataset.tab).classList.add("active");
  });
});

/* ── summary ── */
const sumEl = document.getElementById("sum-text");
if (data.summary && data.summary.trim()) {
  sumEl.textContent = data.summary;
} else {
  sumEl.innerHTML = `<span class="sum-empty">No integrated summary available.</span>`;
}

/* ── wire controls ── */
for (const el of Object.values(controls)) el.addEventListener("change", render);
search.addEventListener("input", render);

window.addEventListener("load", function(){
  computeLayout();
  render();
});'
}

# ── R public function ─────────────────────────────────────────

#' Generate an interactive hypothesis-level synthesis network HTML
#'
#' @param synthesis A \code{res$module_synthesis} list, or a featuremap result
#'   object containing \code{module_synthesis}.
#' @param study_context Optional list with \code{species}, \code{biospecimen},
#'   \code{phenotype} to display in the page header.
#' @param output_dir Directory for the output HTML.
#' @param output_file File name for the output HTML.
#' @param show_same_module Ignored (same-module links are never rendered).
#' @return Invisibly, the written HTML path.
#' @export
generate_synthesis_network_html <- function(synthesis,
                                            study_context    = NULL,
                                            output_dir       = getwd(),
                                            output_file      = "synthesis_network.html",
                                            show_same_module = FALSE) {
  synthesis <- .syn_get_synthesis(synthesis)
  if (is.null(synthesis$nodes) || !is.data.frame(synthesis$nodes))
    stop("generate_synthesis_network_html() requires synthesis$nodes.")

  nodes <- synthesis$nodes
  relationships <- synthesis$relationships %||% synthesis$relationships_sparse
  if (is.null(relationships) || !is.data.frame(relationships)) {
    relationships <- data.frame(
      node_1 = character(), node_2 = character(),
      relation_class = character(), subtype = character(),
      rationale = character(), stringsAsFactors = FALSE
    )
  }

  nodes <- nodes[nodes$verdict_status == "accepted", , drop = FALSE]
  vn    <- nodes$node_id
  relationships <- relationships[
    relationships$node_1 %in% vn &
    relationships$node_2 %in% vn &
    relationships$relation_class %in% c("direct","indirect"),
    , drop = FALSE
  ]

  graph_json <- jsonlite::toJSON(
    list(nodes   = .syn_json_records(nodes),
         edges   = .syn_json_records(relationships),
         summary = .syn_scalar(synthesis$summary, "")),
    auto_unbox = TRUE, dataframe = "rows", na = "null"
  )
  js <- sub("GRAPH_DATA", graph_json, .syn_js(), fixed = TRUE)

  html <- sprintf(
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Synthesis Network</title>
<style>%s</style>
</head>
<body>

<div class="layout">

  <!-- network -->
  <div class="panel">
    <div class="toolbar">
      <label><input type="checkbox" id="fDirect"   checked> Direct edges</label>
      <label><input type="checkbox" id="fIndirect" checked> Indirect edges</label>
      <input type="text" id="search" placeholder="Search module, hypothesis, title&hellip;">
    </div>
    <div class="legend">
      <span><i class="leg-circle"></i>Node</span>
      <i class="leg-sep"></i>
      <span>Size&nbsp;&rarr;&nbsp;confidence</span>
      <i class="leg-sep"></i>
      <span><i class="leg-edge-d"></i>Direct</span>
      <span><i class="leg-edge-i"></i>Indirect</span>
      <i class="leg-sep"></i>
      <div id="module-legend"></div>
    </div>
    <div class="network-wrap">
      <svg id="network"><g id="edges"></g><g id="nodes"></g></svg>
      <div id="tooltip"></div>
      <div id="pinned"></div>
    </div>
  </div>

  <!-- summary -->
  <div class="panel sum-panel">
    <div class="sum-label">Integrated Summary</div>
    <div class="sum-text" id="sum-text"></div>
  </div>

</div>

<!-- node / edge tables with tabs -->
<div class="table-wrap">
  <div class="tab-bar">
    <button class="tab-btn active" data-tab="nodes">Nodes</button>
    <button class="tab-btn" data-tab="edges">Edges</button>
  </div>

  <div class="tab-panel active" id="tab-nodes">
    <table class="node-tbl">
      <thead>
        <tr>
          <th>Hypothesis</th>
          <th>Module</th>
          <th>Type</th>
          <th>Confidence</th>
          <th>Title</th>
        </tr>
      </thead>
      <tbody id="tbl-body"></tbody>
    </table>
  </div>

  <div class="tab-panel" id="tab-edges">
    <table class="node-tbl">
      <thead>
        <tr>
          <th>Node 1</th>
          <th>Node 2</th>
          <th>Relation</th>
          <th>Subtype</th>
          <th>Rationale</th>
        </tr>
      </thead>
      <tbody id="etbl-body"></tbody>
    </table>
  </div>
</div>

<script>%s</script>
</body>
</html>',
    .syn_css(), js
  )

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, output_file)
  writeLines(html, out_path, useBytes = FALSE)
  message(sprintf("[HTML] Synthesis network -> %s", out_path))
  invisible(out_path)
}

#' Export an interactive synthesis network from a featuremap result
#'
#' @param res A featuremap result containing \code{module_synthesis}.
#' @param html_dir Directory for the output HTML.
#' @param output_file File name for the output HTML.
#' @param show_same_module Ignored (same-module links are never rendered).
#' @param output_dir Deprecated alias for \code{html_dir}.
#' @return Invisibly, the written HTML path.
#' @export
export_synthesis_network_html <- function(res,
                                          html_dir    = getwd(),
                                          output_file = "synthesis_network.html",
                                          show_same_module = FALSE,
                                          output_dir  = html_dir) {
  if (is.null(res$module_synthesis))
    stop("res$module_synthesis is missing. Run synthesize_modules() first.")
  if (!missing(output_dir)) html_dir <- output_dir
  generate_synthesis_network_html(
    synthesis        = res$module_synthesis,
    study_context    = res$study_context,
    output_dir       = html_dir,
    output_file      = output_file,
    show_same_module = show_same_module
  )
}

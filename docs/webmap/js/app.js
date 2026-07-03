// app.js — bootstrap and wiring for the SY Companies House firm map.
import { loadData, firmWebsite } from './data.js';
import { Filters } from './filters.js';
import { MapView, fmtInt, fmtPct, fmtAge } from './map.js';
import { TreemapView } from './treemap.js';
import { DistView } from './dist.js';
import { exportCsv } from './export.js';

const DATA_URL = 'data/companieshouse_sy.csv';
const BOUNDARIES_URL = 'data/sy_localauthorities.geojson';

const $ = (id) => document.getElementById(id);
const state = { metric: 'count', tmMetric: 'emp', selection: null, selectedRows: null, additive: false, plotted: [], distKind: 'hist', cbFriendly: true };

let rows, headers, filters, map, treemap, dist;

init();

async function init() {
  try {
    const loaded = await loadData(DATA_URL);
    rows = loaded.rows;
    headers = loaded.headers;

    // LA boundary lines (optional overlay; map still works if the file is missing)
    const boundaries = await fetch(BOUNDARIES_URL).then(r => (r.ok ? r.json() : null)).catch(() => null);

    map = new MapView({
      divId: 'map',
      meta: loaded.meta,
      boundaries,
      onSelect: (sel) => applySelection(sel),
      onFirmClick: (row, ev) => showFirmPopup(row, ev),
    });

    treemap = new TreemapView({
      divId: 'treemap',
      sectionColors: loaded.sectionColors,
      onSelectSector: (level, code) => { if (!level) filters.clearSic(); else filters.setSicSelection(level, [code]); },
    });

    dist = new DistView({ divId: 'distChart' });

    filters = new Filters({
      rows,
      sicIndex: loaded.sicIndex,
      meta: loaded.meta,
      onChange: refresh,
    });

    wireControls();
    refresh();
    $('loading').hidden = true;
  } catch (e) {
    console.error(e);
    $('loadingMsg').textContent = 'Failed to load data: ' + (e && e.message ? e.message : e);
  }
}

// Recompute filtered sets and redraw map + treemap.
function refresh() {
  const base = filters.apply(rows, true);           // all filters incl. SIC selection
  state.plotted = state.metric === 'pct' ? base.filter(r => r.pct != null) : base;

  map.render(state.plotted, state.metric);
  treemap.render(base, state.tmMetric, filters.currentTreemapLevel()); // treemap reflects + zooms to the SIC selection

  state.selectedRows = null;       // a filter change drops any accumulated box/lasso selection
  map.clearSelection();            // and clear its on-map highlight (uirevision would otherwise keep it lit)
  state.selection = state.plotted; // default: all filtered firms are "selected" (ready to export); box/lasso narrows further
  updateCounts();
  updateDist();
  resizePlots();
}

function updateCounts() {
  const sel = state.selection || [];
  const emp = sel.reduce((s, r) => s + (r.employees || 0), 0);
  $('countSel').textContent = sel.length.toLocaleString();
  $('countSelEmp').textContent = emp.toLocaleString();
}

// Redraw the bottom-left distribution overlay: the current filtered set ("all shown")
// as the baseline, with the explicit box/lasso selection (if any) overlaid on top.
function updateDist() {
  if (!dist) return;
  const panel = $('distPanel');
  if (panel.hidden) return;                          // feature switched off (see index.html)
  $('distTitle').textContent = state.metric === 'pct' ? '% change (YoY)' : 'Employee count';
  if (panel.classList.contains('collapsed')) return;
  dist.render(state.plotted, state.selectedRows, state.metric, state.distKind);
}

// Apply a box/lasso result. In "add to selection" mode the new points are unioned
// (by row identity) with the running selection and the whole union is re-highlighted;
// otherwise each selection replaces the last. state.selectedRows is the explicit
// selection (null = none), kept distinct from the "default = all plotted" fallback.
function applySelection(sel) {
  if (sel && sel.length) {
    if (state.additive && state.selectedRows) {
      const set = new Set(state.selectedRows);
      sel.forEach(r => set.add(r));
      state.selectedRows = [...set];
    } else {
      state.selectedRows = sel;
    }
    map.highlight(state.selectedRows);
  } else if (state.additive) {
    return;                       // in add mode, ignore an empty/deselect so a stray click can't wipe the set
  } else {
    state.selectedRows = null;    // a plain deselect clears back to the default
    map.clearSelection();
  }
  state.selection = state.selectedRows && state.selectedRows.length ? state.selectedRows : state.plotted;
  updateCounts();
  updateDist();
}

function wireControls() {
  // size/colour metric
  document.querySelectorAll('input[name="metric"]').forEach(r =>
    r.addEventListener('change', () => { if (r.checked) { state.metric = r.value; refresh(); } }));

  // treemap size metric (redraw treemap only; keeps map selection)
  document.querySelectorAll('input[name="tmMetric"]').forEach(r =>
    r.addEventListener('change', () => {
      if (r.checked) { state.tmMetric = r.value; treemap.render(filters.apply(rows, true), state.tmMetric, filters.currentTreemapLevel()); }
    }));

  // map tool buttons
  const toolBtns = document.querySelectorAll('#tools .tool');
  toolBtns.forEach(btn => btn.addEventListener('click', () => {
    toolBtns.forEach(b => b.classList.remove('active'));
    btn.classList.add('active');
    map.setDragMode(btn.dataset.mode);
  }));
  // Default (no active tool) is pan + scroll-zoom, set by map.mode in the constructor.

  // on-map +/- zoom control
  $('zoomInBtn').addEventListener('click', () => map.zoomBy(1));
  $('zoomOutBtn').addEventListener('click', () => map.zoomBy(-1));

  // colour-blind palette toggle (eye button above the map colour bar)
  $('paletteToggle').addEventListener('click', () => {
    state.cbFriendly = !state.cbFriendly;
    const btn = $('paletteToggle');
    btn.classList.toggle('on', state.cbFriendly);
    btn.setAttribute('aria-pressed', String(state.cbFriendly));
    btn.title = state.cbFriendly ? 'Switch back to the default palette' : 'Switch to a colour-blind-friendly palette';
    map.setColorBlind(state.cbFriendly);
  });

  // "add to selection" toggle — when on, each box/lasso selection accumulates
  $('addSelBtn').addEventListener('click', () => {
    state.additive = !state.additive;
    $('addSelBtn').classList.toggle('on', state.additive);
    $('addSelBtn').setAttribute('aria-pressed', String(state.additive));
  });

  $('clearSelBtn').addEventListener('click', () => {
    map.clearSelection();
    map.setDragMode('pan');                        // leave box-select / lasso, restore drag-to-pan
    toolBtns.forEach(b => b.classList.remove('active'));
    state.selectedRows = null;
    state.selection = state.plotted; // reset to the full filtered set (the default)
    updateCounts();
    updateDist();
  });

  $('exportBtn').addEventListener('click', () => {
    const out = state.selection && state.selection.length ? state.selection : state.plotted;
    if (!out.length) { alert('Nothing to export — no firms are shown.'); return; }
    exportCsv(out, headers);
  });

  // distribution overlay: bars/density view + show/hide
  document.querySelectorAll('input[name="distKind"]').forEach(r =>
    r.addEventListener('change', () => { if (r.checked) { state.distKind = r.value; updateDist(); } }));
  $('distToggle').addEventListener('click', () => {
    const panel = $('distPanel');
    panel.classList.toggle('collapsed');
    $('distToggle').textContent = panel.classList.contains('collapsed') ? 'show' : 'hide';
    updateDist();
    resizePlots();
  });

  // treemap panel show/hide
  $('tmToggle').addEventListener('click', () => {
    const panel = $('treemapPanel');
    panel.classList.toggle('collapsed');
    $('tmToggle').textContent = panel.classList.contains('collapsed') ? 'show' : 'hide';
    resizePlots();
  });

  // firm popup close
  $('firmPopupClose').addEventListener('click', () => { $('firmPopup').hidden = true; });
  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') { $('firmPopup').hidden = true; $('infoModal').hidden = true; }
  });

  // info / about modal — content loaded from info.html on first open, then cached
  let infoLoaded = false;
  $('infoBtn').addEventListener('click', async () => {
    if (!infoLoaded) {
      $('infoBody').innerHTML = await fetch('info.html')
        .then(r => (r.ok ? r.text() : Promise.reject(new Error('HTTP ' + r.status))))
        .catch(() => '<p>Could not load information.</p>');
      infoLoaded = true;
    }
    $('infoModal').hidden = false;
  });
  const closeInfo = () => { $('infoModal').hidden = true; };
  $('infoClose').addEventListener('click', closeInfo);
  $('infoBackdrop').addEventListener('click', closeInfo);

  window.addEventListener('resize', resizePlots);
}

function resizePlots() {
  requestAnimationFrame(() => {
    if (map && map.div) Plotly.Plots.resize(map.div);
    const tm = $('treemap');
    if (treemap && !$('treemapPanel').classList.contains('collapsed')) Plotly.Plots.resize(tm);
    if (dist && dist._drawn && !$('distPanel').classList.contains('collapsed')) Plotly.Plots.resize(dist.div);
  });
}

function showFirmPopup(row, mouseEv) {
  const body = $('firmPopupBody');
  body.replaceChildren();

  const name = row.CompanyName || '(no name)';
  const site = firmWebsite(row.website);
  const h = document.createElement('h3');
  if (site) {
    const a = document.createElement('a');
    a.href = site; a.target = '_blank'; a.rel = 'noopener';
    a.textContent = name;
    a.title = site;
    h.append(a);
  } else {
    h.textContent = name;
  }
  body.append(h);

  const kv = (label, value, cls) => {
    const d = document.createElement('div'); d.className = 'kv';
    const a = document.createElement('span'); a.textContent = label;
    const b = document.createElement('span'); if (cls) b.className = cls; b.textContent = value;
    d.append(a, b); body.append(d);
  };
  kv('Employees (this year)', fmtInt(row.employees));
  kv('Employees (last year)', fmtInt(row.employeesLast));
  kv('Change YoY', fmtPct(row.pct), row.pct == null ? '' : (row.pct >= 0 ? 'pos' : 'neg'));
  kv('Age (years)', fmtAge(row.age));
  kv('Local authority', row.localauthority_name || '—');
  kv('SIC (5-digit)', row.SIC_5DIGIT_NAME || '—');

  const link = document.createElement('a');
  link.href = 'https://find-and-update.company-information.service.gov.uk/company/' + encodeURIComponent(row.CompanyNumber);
  link.target = '_blank'; link.rel = 'noopener';
  link.textContent = 'Companies House page ↗';
  const p = document.createElement('div'); p.style.marginTop = '8px'; p.append(link);
  body.append(p);

  const pop = $('firmPopup');
  pop.hidden = false;
  const x = mouseEv ? mouseEv.clientX : 120;
  const y = mouseEv ? mouseEv.clientY : 120;
  const pw = 320, ph = pop.offsetHeight || 220;
  pop.style.left = Math.max(8, Math.min(x + 12, window.innerWidth - pw - 10)) + 'px';
  pop.style.top = Math.max(8, Math.min(y + 12, window.innerHeight - ph - 10)) + 'px';
}

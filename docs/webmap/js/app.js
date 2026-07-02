// app.js — bootstrap and wiring for the SY Companies House firm map.
import { loadData, firmWebsite } from './data.js';
import { Filters } from './filters.js';
import { MapView, fmtInt, fmtPct, fmtAge } from './map.js';
import { TreemapView } from './treemap.js';
import { exportCsv } from './export.js';

const DATA_URL = 'data/companieshouse_sy.csv';
const BOUNDARIES_URL = 'data/sy_localauthorities.geojson';

const $ = (id) => document.getElementById(id);
const state = { metric: 'count', tmMetric: 'emp', selection: null, plotted: [] };

let rows, headers, filters, map, treemap;

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
      onSelect: (sel) => { state.selection = sel && sel.length ? sel : state.plotted; updateCounts(); },
      onFirmClick: (row, ev) => showFirmPopup(row, ev),
    });

    treemap = new TreemapView({
      divId: 'treemap',
      sectionColors: loaded.sectionColors,
      onSelectSector: (level, code) => { if (!level) filters.clearSic(); else filters.setSicSelection(level, [code]); },
    });

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

  state.selection = state.plotted; // default: all filtered firms are "selected" (ready to export); box/lasso narrows further
  updateCounts();
  resizePlots();
}

function updateCounts() {
  const shown = state.plotted;
  const emp = shown.reduce((s, r) => s + (r.employees || 0), 0);
  $('countShown').textContent = shown.length.toLocaleString();
  $('countEmp').textContent = emp.toLocaleString();
  $('countSel').textContent = (state.selection ? state.selection.length : 0).toLocaleString();
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
  document.querySelector('#tools .tool[data-mode="pan"]').classList.add('active');

  $('clearSelBtn').addEventListener('click', () => {
    map.clearSelection();
    state.selection = state.plotted; // reset to the full filtered set (the default)
    updateCounts();
  });

  $('exportBtn').addEventListener('click', () => {
    const out = state.selection && state.selection.length ? state.selection : state.plotted;
    if (!out.length) { alert('Nothing to export — no firms are shown.'); return; }
    exportCsv(out, headers);
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
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') $('firmPopup').hidden = true; });

  window.addEventListener('resize', resizePlots);
}

function resizePlots() {
  requestAnimationFrame(() => {
    if (map && map.div) Plotly.Plots.resize(map.div);
    const tm = $('treemap');
    if (treemap && !$('treemapPanel').classList.contains('collapsed')) Plotly.Plots.resize(tm);
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

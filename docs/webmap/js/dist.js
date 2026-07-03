// dist.js — bottom-left overlay comparing the current box/lasso selection's
// distribution against the whole currently-filtered set ("all shown"). Two views
// (overlaid bars / overlaid density curves), both area-normalised so the small
// selection is comparable in shape to the larger baseline. Uses the global `Plotly`.

const ACCENT = '#2b6cb0';
const GREY = '#6b7482';
const GAUSS = (u) => Math.exp(-0.5 * u * u) / Math.sqrt(2 * Math.PI);

// Nice employee tick values for the log x-axis (mirrors map.js logTicks): labels
// read as real employee counts, placed at their log10 positions.
function logTicks(loLog, hiLog) {
  const nice = [1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000];
  const lo = Math.pow(10, loLog), hi = Math.pow(10, hiLog);
  let vals = nice.filter(v => v >= lo * 0.999 && v <= hi * 1.001);
  if (vals.length < 2) vals = [Math.max(1, Math.round(lo)), Math.max(2, Math.round(hi))];
  return { tickvals: vals.map(v => Math.log10(Math.max(v, 1))), ticktext: vals.map(v => v.toLocaleString()) };
}

function percentile(sorted, p) {
  if (!sorted.length) return 0;
  return sorted[Math.min(sorted.length - 1, Math.floor(p * sorted.length))];
}

// Working-space values for a metric (skipping nulls). Employee counts are taken in
// log10 space so the long right tail doesn't collapse into one spike at zero.
function valuesFor(rows, metric) {
  const out = [];
  if (metric === 'pct') {
    for (const r of rows) if (r.pct != null && !Number.isNaN(r.pct)) out.push(r.pct);
  } else {
    for (const r of rows) if (r.employees != null && !Number.isNaN(r.employees)) out.push(Math.log10(Math.max(r.employees, 1)));
  }
  return out;
}

// Silverman-style bandwidth, floored by the caller so a huge n can't make it spiky.
function bandwidthOf(values) {
  const n = values.length;
  if (n < 2) return 1;
  let mean = 0; for (const v of values) mean += v; mean /= n;
  let varc = 0; for (const v of values) varc += (v - mean) ** 2; varc /= n;
  const sd = Math.sqrt(varc) || 1;
  return 0.9 * sd * Math.pow(n, -1 / 5);
}

// Area-normalised coarse histogram (for bars) + a binned-Gaussian-KDE curve (for the
// density view), both over a fixed [domain] shared by baseline and selection so they
// overlay on one axis. The KDE is convolved off a fine histogram, so its cost is
// O(fineBins × grid) — independent of how many firms are in `values`.
function series(values, domain, bw, coarseBins, fineBins, kdeGrid) {
  const [d0, d1] = domain;
  const span = (d1 - d0) || 1;
  const n = values.length;
  const cw = span / coarseBins;
  const fw = span / fineBins;
  const coarse = new Array(coarseBins).fill(0);
  const fine = new Array(fineBins).fill(0);
  for (const v of values) {
    let cb = Math.floor((v - d0) / cw); if (cb < 0) cb = 0; else if (cb >= coarseBins) cb = coarseBins - 1;
    let fb = Math.floor((v - d0) / fw); if (fb < 0) fb = 0; else if (fb >= fineBins) fb = fineBins - 1;
    coarse[cb]++; fine[fb]++;
  }
  const centers = [], density = [];
  for (let i = 0; i < coarseBins; i++) {
    centers.push(d0 + (i + 0.5) * cw);
    density.push(n ? coarse[i] / (n * cw) : 0);
  }
  const fineCenters = [];
  for (let i = 0; i < fineBins; i++) fineCenters.push(d0 + (i + 0.5) * fw);
  const kdeX = [], kdeY = [];
  for (let g = 0; g < kdeGrid; g++) {
    const x = d0 + (span * g) / (kdeGrid - 1);
    let s = 0;
    for (let i = 0; i < fineBins; i++) if (fine[i]) s += fine[i] * GAUSS((x - fineCenters[i]) / bw);
    kdeX.push(x);
    kdeY.push(n ? s / (n * bw) : 0);
  }
  return { centers, density, kdeX, kdeY, n, binWidth: cw };
}

export class DistView {
  constructor({ divId }) {
    this.div = document.getElementById(divId);
    this._drawn = false;
  }

  // reference: the "all shown" baseline (current filtered set). selection: the explicit
  // box/lasso rows, or null when nothing is selected. metric: 'count' | 'pct'.
  // kind: 'hist' | 'density'.
  render(reference, selection, metric, kind) {
    const refVals = valuesFor(reference || [], metric);
    if (!refVals.length) { this._empty(); return; }

    // Shared domain (from the baseline; the selection is always a subset so it fits).
    let domain, xaxis;
    if (metric === 'pct') {
      const sorted = refVals.slice().sort((a, b) => a - b);
      let lo = Math.min(percentile(sorted, 0.02), 0);   // robust bounds, always spanning 0
      let hi = Math.max(percentile(sorted, 0.98), 0);
      if (hi - lo < 1) hi = lo + 1;
      domain = [lo, hi];
      xaxis = { range: domain, ticksuffix: '%', zeroline: true, zerolinecolor: '#b9c1cc', tickfont: { size: 9 }, showgrid: false };
    } else {
      let lo = Infinity, hi = -Infinity;
      for (const v of refVals) { if (v < lo) lo = v; if (v > hi) hi = v; }
      if (lo === Infinity) { lo = 0; hi = 1; }
      if (hi - lo < 0.3) hi = lo + 0.3;
      domain = [lo, hi];
      const { tickvals, ticktext } = logTicks(lo, hi);
      xaxis = { range: domain, tickvals, ticktext, tickfont: { size: 9 }, showgrid: false };
    }

    const bw = Math.max(bandwidthOf(refVals), (domain[1] - domain[0]) / 40);
    const COARSE = 30, FINE = 120, GRID = 96;

    const ref = series(refVals, domain, bw, COARSE, FINE, GRID);
    const selVals = selection && selection.length ? valuesFor(selection, metric) : null;
    const sel = selVals && selVals.length ? series(selVals, domain, bw, COARSE, FINE, GRID) : null;

    const traces = [];
    if (kind === 'density') {
      traces.push({ type: 'scatter', x: ref.kdeX, y: ref.kdeY, mode: 'lines', name: 'all shown',
        line: { color: GREY, width: 1.5 }, fill: 'tozeroy', fillcolor: 'rgba(107,116,130,.18)', hoverinfo: 'skip' });
      if (sel) traces.push({ type: 'scatter', x: sel.kdeX, y: sel.kdeY, mode: 'lines', name: 'selection',
        line: { color: ACCENT, width: 2 }, fill: 'tozeroy', fillcolor: 'rgba(43,108,176,.22)', hoverinfo: 'skip' });
    } else {
      traces.push({ type: 'bar', x: ref.centers, y: ref.density, name: 'all shown', width: ref.binWidth,
        marker: { color: 'rgba(107,116,130,.35)' }, hoverinfo: 'skip' });
      if (sel) traces.push({ type: 'bar', x: sel.centers, y: sel.density, name: 'selection', width: sel.binWidth,
        marker: { color: 'rgba(43,108,176,.55)' }, hoverinfo: 'skip' });
    }

    // Cap the y-range so a tiny (spiky) selection can't flatten the baseline curve.
    const peak = (s) => Math.max(kind === 'density' ? Math.max(...s.kdeY) : Math.max(...s.density), 0);
    const refPeak = peak(ref);
    const yMax = (sel ? Math.max(refPeak, Math.min(peak(sel), refPeak * 2.5)) : refPeak) * 1.08 || 1;

    const layout = {
      margin: { l: 6, r: 6, t: 4, b: 20 },
      barmode: 'overlay', bargap: 0.04,
      xaxis,
      yaxis: { range: [0, yMax], showticklabels: false, showgrid: false, zeroline: false },
      showlegend: !!sel,
      legend: { x: 0.98, y: 1, xanchor: 'right', yanchor: 'top', font: { size: 9 },
        bgcolor: 'rgba(255,255,255,.6)', borderwidth: 0, tracegroupgap: 0 },
      paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)',
      font: { size: 9 },
    };

    Plotly.react(this.div, traces, layout, { displayModeBar: false, staticPlot: true, responsive: true });
    this._drawn = true;
  }

  _empty() {
    Plotly.react(this.div, [], {
      margin: { l: 6, r: 6, t: 4, b: 4 },
      xaxis: { visible: false }, yaxis: { visible: false },
      annotations: [{ text: 'no firms shown', showarrow: false, font: { size: 10, color: '#8a93a0' },
        x: 0.5, y: 0.5, xref: 'paper', yref: 'paper' }],
      paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)',
    }, { displayModeBar: false, staticPlot: true, responsive: true });
    this._drawn = true;
  }
}

// map.js — the Plotly scattermapbox firm map.
// Uses the global `Plotly` loaded via CDN in index.html.

const RDYLGN = [[0, '#c62828'], [0.5, '#fff3b0'], [1, '#1a7f37']]; // diverging: red -> yellow -> green

// Colour-blind-friendly alternatives, toggled by the eye button above the colour bar.
//   employee count -> Viridis (perceptually uniform sequential)
//   % change YoY   -> PuOr diverging (orange = shrank, purple = grew) which avoids the
//                     red/green confusion of the default while keeping 0 = neutral white.
const PUOR = [
  [0.0, '#b35806'], [0.125, '#e08214'], [0.25, '#fdb863'], [0.375, '#fee0b6'],
  [0.5, '#f7f7f7'],
  [0.625, '#d8daeb'], [0.75, '#b2abd2'], [0.875, '#8073ac'], [1.0, '#542788'],
];
const SCALES = {
  count: { default: 'Portland', cb: 'Viridis' },
  pct:   { default: RDYLGN,     cb: PUOR },
};
const fmtInt = (v) => (v == null ? '—' : Number(v).toLocaleString());
const fmtPct = (v) => (v == null ? '—' : (v >= 0 ? '+' : '') + v.toFixed(1) + '%');
const fmtAge = (v) => (v == null ? '—' : v.toFixed(1));

// Min/max of a value across the current rows, for scaling the colour bar to the
// selection. Falls back to [0,1] when empty and nudges a degenerate single value.
function extent(rows, get) {
  let lo = Infinity, hi = -Infinity;
  for (const r of rows) {
    const v = get(r);
    if (v == null || Number.isNaN(v)) continue;
    if (v < lo) lo = v;
    if (v > hi) hi = v;
  }
  if (lo === Infinity) return [0, 1];
  if (lo === hi) return [lo, hi + 1];
  return [lo, hi];
}

// The p-th percentile of |value| across rows. Used as a robust cap for the
// diverging % change scale so a few extreme outliers don't dominate it.
function absPercentile(rows, get, p) {
  const arr = [];
  for (const r of rows) { const v = get(r); if (v == null || Number.isNaN(v)) continue; arr.push(Math.abs(v)); }
  if (!arr.length) return 1;
  arr.sort((a, b) => a - b);
  return arr[Math.min(arr.length - 1, Math.floor(p * arr.length))] || 1;
}

// Colour-bar ticks for a log-scaled count: round employee values within [lo,hi],
// placed at their log positions and labelled with the real number.
function logTicks(lo, hi) {
  const nice = [1, 2, 3, 5, 10, 20, 30, 50, 100, 200, 300, 500, 1000, 2000, 5000, 10000];
  let vals = nice.filter(v => v >= lo && v <= hi);
  if (vals.length < 2) vals = [Math.max(1, Math.round(lo)), Math.max(2, Math.round(hi))];
  return { tickvals: vals.map(v => Math.log10(Math.max(v, 1))), ticktext: vals.map(v => v.toLocaleString()) };
}

export class MapView {
  constructor({ divId, meta, onSelect, onFirmClick, boundaries }) {
    this.div = document.getElementById(divId);
    this.meta = meta;
    this.onSelect = onSelect;
    this.onFirmClick = onFirmClick;
    this.mode = 'pan';
    this.currentRows = [];
    this._drawn = false;
    this._cb = true;           // colour-blind-friendly palette on/off (on by default)
    this._metric = 'count';    // metric of the last render (so a palette swap can restyle)
    // LA boundary lines drawn under the points (a stable reference so Plotly.react
    // doesn't re-diff the GeoJSON on every filter). Empty if the file didn't load.
    this._boundaryLayers = boundaries
      ? [{ sourcetype: 'geojson', source: boundaries, type: 'line', color: '#3a3f4a', opacity: 0.85, line: { width: 1.5 } }]
      : [];
  }

  _markerSize(rows, metric) {
    if (metric === 'pct') {
      const ref = Math.sqrt(Math.max(this.meta.pctRef, 10));
      return rows.map(r => 4 + 20 * Math.min(1, Math.sqrt(Math.abs(r.pct ?? 0)) / ref));
    }
    const ref = Math.sqrt(Math.max(this.meta.empRef, 1));
    return rows.map(r => 4 + 22 * Math.min(1, Math.sqrt(r.employees ?? 0) / ref));
  }

  // Colourscale for a metric, honouring the colour-blind toggle.
  _scaleFor(metric) {
    const set = metric === 'pct' ? SCALES.pct : SCALES.count;
    return this._cb ? set.cb : set.default;
  }

  _markerColorSpec(rows, metric) {
    // Scale the colour bar to the min/max of the current selection (both metrics).
    if (metric === 'pct') {
      // 0-centred, symmetric, robust cap: keeps 0 = neutral, low = shrank / high = grew.
      const cap = Math.max(absPercentile(rows, r => r.pct, 0.95), 1);
      return {
        color: rows.map(r => r.pct),
        colorscale: this._scaleFor('pct'), cmin: -cap, cmax: cap,
        colorbar: { title: { text: 'Δ % YoY', side: 'right' }, thickness: 12, len: 0.6, x: 1, xpad: 2 },
      };
    }
    // Log-compressed so the many small firms spread across the palette instead of
    // all sitting at the low (yellow) end. cmin/cmax still track the selection.
    const [lo, hi] = extent(rows, r => r.employees);
    const cmin = Math.log10(Math.max(lo, 1));
    let cmax = Math.log10(Math.max(hi, 1));
    if (cmax <= cmin) cmax = cmin + 1;
    const { tickvals, ticktext } = logTicks(lo, hi);
    return {
      color: rows.map(r => (r.employees == null ? null : Math.log10(Math.max(r.employees, 1)))),
      colorscale: this._scaleFor('count'), cmin, cmax,
      colorbar: { title: { text: 'Employees', side: 'right' }, thickness: 12, len: 0.6, x: 1, xpad: 2, tickvals, ticktext },
    };
  }

  render(rows, metric) {
    this.currentRows = rows;
    this._metric = metric;
    const colorSpec = this._markerColorSpec(rows, metric);
    const customdata = rows.map(r => [
      r.CompanyName || '(no name)',
      fmtInt(r.employees), fmtPct(r.pct), fmtAge(r.age),
      r.SIC_SECTION_NAME || '', r.localauthority_name || '',
    ]);

    const trace = {
      type: 'scattermapbox',
      lon: rows.map(r => r.lon),
      lat: rows.map(r => r.lat),
      mode: 'markers',
      marker: {
        size: this._markerSize(rows, metric),
        opacity: 0.8,
        ...colorSpec,
      },
      customdata,
      hovertemplate:
        '<b>%{customdata[0]}</b><br>' +
        'Employees: %{customdata[1]} (Δ %{customdata[2]})<br>' +
        'Age: %{customdata[3]} yrs · %{customdata[4]}<br>' +
        '<span style="color:#888">%{customdata[5]}</span>' +
        '<extra></extra>',
      unselected: { marker: { opacity: 0.12 } },
      selected: { marker: { opacity: 0.95 } },
    };

    const layout = {
      dragmode: this.mode, // 'pan' | 'zoom' | 'select' | 'lasso'
      margin: { l: 0, r: 0, t: 0, b: 0 },
      uirevision: 'keep', // preserve pan/zoom across re-renders (filtering/metric change)
      mapbox: {
        style: 'carto-positron',
        center: { lon: -1.30, lat: 53.46 },
        zoom: 9.2,
        layers: this._boundaryLayers, // LA borders, rendered beneath the marker trace
      },
      showlegend: false,
    };

    const config = { responsive: true, scrollZoom: true, displayModeBar: false };

    Plotly.react(this.div, [trace], layout, config);

    if (!this._drawn) {
      this._drawn = true;
      this.div.on('plotly_selected', (ev) => {
        if (!ev || !ev.points) { this.onSelect(null); return; }
        this.onSelect(ev.points.map(p => this.currentRows[p.pointNumber]).filter(Boolean));
      });
      this.div.on('plotly_deselect', () => this.onSelect(null));
      this.div.on('plotly_click', (ev) => {
        if (!ev || !ev.points || !ev.points.length) return;
        const row = this.currentRows[ev.points[0].pointNumber];
        if (row) this.onFirmClick(row, ev.event);
      });
    }
  }

  setDragMode(mode) {
    this.mode = mode;
    Plotly.relayout(this.div, { dragmode: mode });
  }

  // Swap between the default palettes and the colour-blind-friendly ones. Only the
  // colourscale changes (data values and cmin/cmax are unchanged), so a single restyle
  // updates the markers and colour bar while leaving pan/zoom and the selection intact.
  setColorBlind(on) {
    this._cb = !!on;
    if (this._drawn) Plotly.restyle(this.div, { 'marker.colorscale': [this._scaleFor(this._metric)] });
  }

  // Step the mapbox zoom (used by the on-map +/- buttons). Reads the live zoom
  // from _fullLayout so it stays in sync after the user pans/scroll-zooms.
  zoomBy(delta) {
    const mb = this.div._fullLayout && this.div._fullLayout.mapbox;
    const cur = mb ? mb.zoom : this.div.layout.mapbox.zoom;
    Plotly.relayout(this.div, { 'mapbox.zoom': cur + delta });
  }

  clearSelection() {
    if (this._drawn) Plotly.restyle(this.div, { selectedpoints: [null] });
  }

  // Light up an explicit set of rows on the map. Used by "add to selection" so the
  // whole accumulated union stays highlighted, not just the most recent box/lasso.
  highlight(rows) {
    if (!this._drawn) return;
    if (!rows || !rows.length) { this.clearSelection(); return; }
    const idx = new Map(this.currentRows.map((r, i) => [r, i]));
    const pts = rows.map(r => idx.get(r)).filter(i => i != null);
    Plotly.restyle(this.div, { selectedpoints: [pts] });
  }
}

export { fmtInt, fmtPct, fmtAge };

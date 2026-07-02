// map.js — the Plotly scattermapbox firm map.
// Uses the global `Plotly` loaded via CDN in index.html.

const RDYLGN = [[0, '#c62828'], [0.5, '#fff3b0'], [1, '#1a7f37']]; // diverging: red -> yellow -> green
const fmtInt = (v) => (v == null ? '—' : Number(v).toLocaleString());
const fmtPct = (v) => (v == null ? '—' : (v >= 0 ? '+' : '') + v.toFixed(1) + '%');
const fmtAge = (v) => (v == null ? '—' : v.toFixed(1));

export class MapView {
  constructor({ divId, meta, onSelect, onFirmClick }) {
    this.div = document.getElementById(divId);
    this.meta = meta;
    this.onSelect = onSelect;
    this.onFirmClick = onFirmClick;
    this.mode = 'pan';
    this.currentRows = [];
    this._drawn = false;
  }

  _markerSize(rows, metric) {
    if (metric === 'pct') {
      const ref = Math.sqrt(Math.max(this.meta.pctRef, 10));
      return rows.map(r => 4 + 20 * Math.min(1, Math.sqrt(Math.abs(r.pct ?? 0)) / ref));
    }
    const ref = Math.sqrt(Math.max(this.meta.empRef, 1));
    return rows.map(r => 4 + 22 * Math.min(1, Math.sqrt(r.employees ?? 0) / ref));
  }

  _markerColorSpec(rows, metric) {
    if (metric === 'pct') {
      const cap = Math.max(this.meta.pctRef, 10);
      return {
        color: rows.map(r => r.pct),
        colorscale: RDYLGN, cmin: -cap, cmid: 0, cmax: cap,
        colorbar: { title: { text: 'Δ % YoY', side: 'right' }, thickness: 12, len: 0.6, x: 1, xpad: 2 },
      };
    }
    return {
      color: rows.map(r => r.employees),
      colorscale: 'YlOrRd', reversescale: true, cmin: 0, cmax: Math.max(this.meta.empRef, 1),
      colorbar: { title: { text: 'Employees', side: 'right' }, thickness: 12, len: 0.6, x: 1, xpad: 2 },
    };
  }

  render(rows, metric) {
    this.currentRows = rows;
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

  clearSelection() {
    if (this._drawn) Plotly.restyle(this.div, { selectedpoints: [null] });
  }
}

export { fmtInt, fmtPct, fmtAge };

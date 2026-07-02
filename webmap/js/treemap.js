// treemap.js — Plotly SIC treemap (section -> 2-digit -> 5-digit) with click-to-filter.
// Uses the global `Plotly` loaded via CDN in index.html.
import { sicLabel } from './data.js';

const val = (r, metric) => (metric === 'emp' ? (r.employees ?? 0) : 1);

export class TreemapView {
  constructor({ divId, sectionColors, onSelectSector }) {
    this.div = document.getElementById(divId);
    this.sectionColors = sectionColors;
    this.onSelectSector = onSelectSector;
    this._drawn = false;
  }

  render(rows, metric) {
    // Aggregate totals per node so branchvalues:'total' stays consistent.
    const sec = new Map();  // letter -> {label, value}
    const d2 = new Map();   // id -> {label, parent, value, letter}
    const d5 = new Map();   // id -> {label, parent, value, letter}

    for (const r of rows) {
      const letter = (r.SIC_SECTION_LETTER || '').trim();
      const c2 = (r.SIC_2DIGIT_CODE || '').trim();
      const c5 = (r.SIC_5DIGIT_CODE || '').trim();
      const v = val(r, metric);

      const secId = `sec::${letter}`;
      const d2Id = `d2::${letter}::${c2}`;
      const d5Id = `d5::${letter}::${c2}::${c5}`;

      if (!sec.has(letter)) sec.set(letter, { label: sicLabel('section', letter, (r.SIC_SECTION_NAME || '').trim()), value: 0 });
      sec.get(letter).value += v;

      if (!d2.has(d2Id)) d2.set(d2Id, { label: sicLabel('d2', c2, (r.SIC_2DIGIT_NAME || '').trim()), parent: secId, value: 0, letter });
      d2.get(d2Id).value += v;

      if (!d5.has(d5Id)) d5.set(d5Id, { label: sicLabel('d5', c5, (r.SIC_5DIGIT_NAME || '').trim()), parent: d2Id, value: 0, letter });
      d5.get(d5Id).value += v;
    }

    const ids = [], labels = [], parents = [], values = [], colors = [];
    const push = (id, label, parent, value, letter) => {
      ids.push(id); labels.push(label); parents.push(parent); values.push(value);
      colors.push(this.sectionColors[letter] || '#9e9e9e');
    };
    for (const [letter, o] of sec) push(`sec::${letter}`, o.label, '', o.value, letter);
    for (const [id, o] of d2) push(id, o.label, o.parent, o.value, o.letter);
    for (const [id, o] of d5) push(id, o.label, o.parent, o.value, o.letter);

    const trace = {
      type: 'treemap',
      ids, labels, parents, values,
      branchvalues: 'total',
      marker: { colors },
      maxdepth: 2,
      pathbar: { visible: true },
      tiling: { pad: 1 },
      hovertemplate: '<b>%{label}</b><br>' + (metric === 'emp' ? 'Employees' : 'Firms') + ': %{value:,}<extra></extra>',
      texttemplate: '%{label}<br>%{value:,}',
    };

    const layout = { margin: { l: 0, r: 0, t: 0, b: 0 }, uirevision: 'tm' };
    Plotly.react(this.div, [trace], layout, { responsive: true, displayModeBar: false });

    if (!this._drawn) {
      this._drawn = true;
      this.div.on('plotly_treemapclick', (ev) => {
        const p = ev && ev.points && ev.points[0];
        if (!p || !p.id) return;
        const parts = String(p.id).split('::');
        if (parts[0] === 'sec') this.onSelectSector('section', parts[1]);
        else if (parts[0] === 'd2') this.onSelectSector('d2', parts[2]);
        else if (parts[0] === 'd5') this.onSelectSector('d5', parts[3]);
        // don't return false -> allow the treemap's own drill-in zoom as well
      });
    }
  }
}

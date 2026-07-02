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
    this._rev = 0;             // bumped whenever we force the zoom level
    this._uirev = 'tm0';       // current treemap uirevision
    this._selfDriven = false;  // set by a click so we don't fight its own zoom animation
  }

  render(rows, metric, desiredLevel = 'ALL') {
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
    const push = (id, label, parent, value, color) => {
      ids.push(id); labels.push(label); parents.push(parent); values.push(value); colors.push(color);
    };
    // Explicit root so there's always a level above the sections to zoom out to;
    // reaching it (nextLevel === 'ALL') means "no SIC filter" and clears the selection.
    let total = 0; for (const o of sec.values()) total += o.value;
    push('ALL', 'All South Yorkshire', '', total, '#e9edf2');
    for (const [letter, o] of sec) push(`sec::${letter}`, o.label, 'ALL', o.value, this.sectionColors[letter] || '#9e9e9e');
    for (const [id, o] of d2) push(id, o.label, o.parent, o.value, this.sectionColors[o.letter] || '#9e9e9e');
    for (const [id, o] of d5) push(id, o.label, o.parent, o.value, this.sectionColors[o.letter] || '#9e9e9e');

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

    // Drive the zoom from the SIC selection so sidebar-select drills in like a click,
    // and clearing zooms back out. Skip right after a click (Plotly is already
    // animating that drill — forcing the level would kill the animation).
    if (this._selfDriven) {
      this._selfDriven = false;
    } else {
      trace.level = desiredLevel;
      this._rev++;
      this._uirev = 'tm' + this._rev;
    }

    const layout = { margin: { l: 0, r: 0, t: 0, b: 0 }, uirevision: this._uirev };
    Plotly.react(this.div, [trace], layout, { responsive: true, displayModeBar: false });

    if (!this._drawn) {
      this._drawn = true;
      this.div.on('plotly_treemapclick', (ev) => {
        // nextLevel = the node we're navigating TO (drill-in target, or parent on
        // zoom-out). 'sec/d2/d5' select that node; 'ALL' or the synthetic root hash
        // mean we've zoomed back to the top -> clear the SIC selection.
        const dest = ev && ev.nextLevel;
        if (dest === undefined) return;
        this._selfDriven = true; // this click animates its own zoom; don't force a level on the ensuing render
        const parts = String(dest).split('::');
        if (parts[0] === 'sec') this.onSelectSector('section', parts[1]);
        else if (parts[0] === 'd2') this.onSelectSector('d2', parts[2]);
        else if (parts[0] === 'd5') this.onSelectSector('d5', parts[3]);
        else this.onSelectSector(null, null);
        // don't return false -> allow the treemap's own drill/zoom animation as well
      });
    }
  }
}

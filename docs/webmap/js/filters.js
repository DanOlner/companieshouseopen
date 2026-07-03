// filters.js — sidebar controls + the filtering predicate.
// Uses the global `noUiSlider` loaded via CDN in index.html.
import { SIC_LEVELS, BAND_ORDER, sicLabel, firmWebsite } from './data.js';

export class Filters {
  constructor({ rows, sicIndex, meta, onChange }) {
    this.rows = rows;
    this.sicIndex = sicIndex;
    this.meta = meta;
    this.onChange = onChange;

    this.state = {
      sicLevel: 'section',
      sicSelected: new Set(),
      bands: new Set(),
      age: [0, meta.maxAge],
      hasWebsite: false,
      pctDir: 'all',          // 'all' | 'neg' (shrank) | 'pos' (grew)
    };

    this.el = {
      sicLevel: document.getElementById('sicLevel'),
      sicSearch: document.getElementById('sicSearch'),
      sicList: document.getElementById('sicList'),
      sicClear: document.getElementById('sicClear'),
      bandList: document.getElementById('bandList'),
      ageSlider: document.getElementById('ageSlider'),
      ageMinLbl: document.getElementById('ageMinLbl'),
      ageMaxLbl: document.getElementById('ageMaxLbl'),
      hasWebsite: document.getElementById('hasWebsite'),
      webCount: document.getElementById('webCount'),
      pctDir: document.getElementById('pctDir'),
      reset: document.getElementById('resetFilters'),
    };

    this._bandCounts = this._computeBandCounts();
    this._buildBandList();
    this._buildAgeSlider();
    this._renderSicList();
    this._showWebCount();
    this._wire();
  }

  _showWebCount() {
    const n = this.rows.reduce((c, r) => c + (firmWebsite(r.website) ? 1 : 0), 0);
    if (this.el.webCount) this.el.webCount.textContent = n.toLocaleString();
  }

  // ---- filtering ----
  // includeSic=false is used for the treemap, so it always shows the full sector
  // breakdown (clicking a tile is what applies the SIC filter to the map).
  apply(rows, includeSic = true) {
    const { bands, age, sicLevel, sicSelected, pctDir } = this.state;
    const codeField = SIC_LEVELS[sicLevel].code;
    const useBands = bands.size > 0;
    const useSic = includeSic && sicSelected.size > 0;
    const [amin, amax] = age;
    const useAge = amin > 0 || amax < this.meta.maxAge;
    const useWeb = this.state.hasWebsite;
    const usePct = pctDir !== 'all';

    const out = [];
    for (const r of rows) {
      if (useBands && !bands.has(r.sizecategory)) continue;
      if (useAge && (r.age == null || r.age < amin || r.age > amax)) continue;
      if (useWeb && !firmWebsite(r.website)) continue;
      // % change filter: null pct (firms with <5 employees last year) drops out
      // whenever a direction is chosen.
      if (usePct && (r.pct == null || (pctDir === 'neg' ? r.pct >= 0 : r.pct <= 0))) continue;
      if (useSic) {
        const c = r[codeField] == null ? '' : String(r[codeField]).trim();
        if (!sicSelected.has(c)) continue;
      }
      out.push(r);
    }
    return out;
  }

  // Called by the treemap to push a selection into the SIC control.
  setSicSelection(level, codes) {
    this.state.sicLevel = level;
    this.state.sicSelected = new Set(codes);
    this.el.sicLevel.value = level;
    this.el.sicSearch.value = '';
    this._renderSicList();
    this.onChange();
  }

  // Called when the treemap zooms back to the top -> clear the SIC selection.
  clearSic() {
    if (this.state.sicSelected.size === 0) return; // nothing selected; avoid a needless refresh
    this.state.sicSelected = new Set();
    this._renderSicList();
    this.onChange();
  }

  // The treemap node id matching the current single selection (else 'ALL'), so the
  // treemap can zoom to it. Multi-select and 3-digit (no treemap tier) -> 'ALL'.
  currentTreemapLevel() {
    const { sicLevel, sicSelected } = this.state;
    if (sicSelected.size !== 1) return 'ALL';
    const code = [...sicSelected][0];
    if (sicLevel === 'section') return `sec::${code}`;
    const field = SIC_LEVELS[sicLevel].code;
    const row = this.rows.find(r => String(r[field] ?? '').trim() === code);
    if (!row) return 'ALL';
    const letter = String(row.SIC_SECTION_LETTER ?? '').trim();
    if (sicLevel === 'd2') return `d2::${letter}::${code}`;
    if (sicLevel === 'd5') return `d5::${letter}::${String(row.SIC_2DIGIT_CODE ?? '').trim()}::${code}`;
    return 'ALL';
  }

  reset() {
    this.state.sicSelected = new Set();
    this.state.bands = new Set();
    this.state.age = [0, this.meta.maxAge];
    this.state.hasWebsite = false;
    this.state.pctDir = 'all';
    this.el.sicSearch.value = '';
    this._renderSicList();
    this.el.bandList.querySelectorAll('input').forEach(i => { i.checked = false; });
    this.el.hasWebsite.checked = false;
    this.el.pctDir.querySelectorAll('.seg').forEach(b => b.classList.toggle('active', b.dataset.pct === 'all'));
    this._ageSlider.set([0, this.meta.maxAge]);
    this.onChange();
  }

  // ---- band checkboxes ----
  _computeBandCounts() {
    const counts = new Map(BAND_ORDER.map(b => [b, 0]));
    for (const r of this.rows) if (counts.has(r.sizecategory)) counts.set(r.sizecategory, counts.get(r.sizecategory) + 1);
    return counts;
  }

  _buildBandList() {
    const frag = document.createDocumentFragment();
    for (const band of BAND_ORDER) {
      const lab = document.createElement('label');
      const cb = document.createElement('input');
      cb.type = 'checkbox'; cb.value = band;
      cb.addEventListener('change', () => {
        if (cb.checked) this.state.bands.add(band); else this.state.bands.delete(band);
        this.onChange();
      });
      const name = document.createElement('span'); name.className = 'cl-name'; name.textContent = band;
      const cnt = document.createElement('span'); cnt.className = 'cl-count'; cnt.textContent = this._bandCounts.get(band).toLocaleString();
      lab.append(cb, name, cnt);
      frag.append(lab);
    }
    this.el.bandList.replaceChildren(frag);
  }

  // ---- age slider ----
  _buildAgeSlider() {
    const max = Math.max(1, this.meta.maxAge);
    this._ageSlider = noUiSlider.create(this.el.ageSlider, {
      start: [0, max], connect: true, step: 1, range: { min: 0, max },
      format: { to: v => Math.round(v), from: v => Number(v) },
    });
    this.el.ageMaxLbl.textContent = max;
    this._ageSlider.on('update', (vals) => {
      this.el.ageMinLbl.textContent = vals[0];
      this.el.ageMaxLbl.textContent = vals[1];
    });
    this._ageSlider.on('change', (vals) => {
      this.state.age = [Number(vals[0]), Number(vals[1])];
      this.onChange();
    });
  }

  // ---- SIC list ----
  _renderSicList() {
    const level = this.state.sicLevel;
    const q = this.el.sicSearch.value.trim().toLowerCase();
    const entries = [...this.sicIndex[level].values()]
      .map(e => ({ ...e, label: sicLabel(level, e.code, e.name) }))
      .sort((a, b) => b.count - a.count);

    const frag = document.createDocumentFragment();
    for (const e of entries) {
      if (q && !e.label.toLowerCase().includes(q)) continue;
      const lab = document.createElement('label');
      const cb = document.createElement('input');
      cb.type = 'checkbox'; cb.value = e.code;
      cb.checked = this.state.sicSelected.has(e.code);
      cb.addEventListener('change', () => {
        if (cb.checked) this.state.sicSelected.add(e.code); else this.state.sicSelected.delete(e.code);
        this.onChange();
      });
      const name = document.createElement('span'); name.className = 'cl-name'; name.textContent = e.label;
      const cnt = document.createElement('span'); cnt.className = 'cl-count'; cnt.textContent = e.count.toLocaleString();
      lab.append(cb, name, cnt);
      frag.append(lab);
    }
    this.el.sicList.replaceChildren(frag);
  }

  _wire() {
    this.el.sicLevel.addEventListener('change', () => {
      this.state.sicLevel = this.el.sicLevel.value;
      this.state.sicSelected = new Set();
      this._renderSicList();
      this.onChange();
    });
    this.el.sicSearch.addEventListener('input', () => this._renderSicList());
    this.el.sicClear.addEventListener('click', () => {
      this.state.sicSelected = new Set();
      this._renderSicList();
      this.onChange();
    });
    this.el.hasWebsite.addEventListener('change', () => {
      this.state.hasWebsite = this.el.hasWebsite.checked;
      this.onChange();
    });
    this.el.pctDir.querySelectorAll('.seg').forEach(btn => btn.addEventListener('click', () => {
      this.state.pctDir = btn.dataset.pct;
      this.el.pctDir.querySelectorAll('.seg').forEach(b => b.classList.toggle('active', b === btn));
      this.onChange();
    }));
    this.el.reset.addEventListener('click', () => this.reset());
  }
}

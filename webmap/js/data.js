// data.js — load, derive and index the Companies House SY CSV.
// Uses the global `Papa` (PapaParse) loaded via CDN in index.html.

export const SIC_LEVELS = {
  section: { code: 'SIC_SECTION_LETTER', name: 'SIC_SECTION_NAME', label: 'Section' },
  d2:      { code: 'SIC_2DIGIT_CODE',    name: 'SIC_2DIGIT_NAME',  label: '2-digit' },
  d3:      { code: 'SIC_3DIGIT_CODE',    name: 'SIC_3DIGIT_NAME',  label: '3-digit' },
  d5:      { code: 'SIC_5DIGIT_CODE',    name: 'SIC_5DIGIT_NAME',  label: '5-digit' },
};

// Employee-count bands exactly as they appear in the `sizecategory` column.
export const BAND_ORDER = ['1', '2-4', '5-9', '10-20', '21-50', '51-100', '101+'];

// 21 reasonably distinct colours, one per SIC section.
const PALETTE = [
  '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2',
  '#7f7f7f', '#bcbd22', '#17becf', '#aec7e8', '#ffbb78', '#98df8a', '#ff9896',
  '#c5b0d5', '#c49c94', '#f7b6d2', '#c7c7c7', '#dbdb8d', '#9edae5', '#393b79',
];

const num = (v) => { const n = parseFloat(v); return Number.isFinite(n) ? n : null; };

const cleanCode = (v) => {
  const c = (v == null ? '' : String(v)).trim();
  return (!c || c.toUpperCase() === 'NA') ? '' : c;
};

// Human label for a SIC code. d2/d3/d5 names already carry a "code : name" prefix;
// the section name does not, so we prepend the letter.
export function sicLabel(levelKey, code, name) {
  if (!code) return '(no SIC code)';
  if (levelKey === 'section') return name ? `${code} — ${name}` : code;
  return name || code;
}

// Normalise a `website` cell to a full URL, or null if the firm has none. The CSV
// stores bare domains (e.g. "www.foo.co.uk") with no scheme.
export function firmWebsite(raw) {
  const w = (raw == null ? '' : String(raw)).trim();
  if (!w || w.toUpperCase() === 'NA') return null;
  return /^https?:\/\//i.test(w) ? w : 'https://' + w;
}

function deriveRow(r) {
  r.employees = num(r.Employees_thisyear);
  r.employeesLast = num(r.Employees_lastyear);
  r.age = num(r.age_of_firm_years);
  r.lon = num(r.lon);
  r.lat = num(r.lat);
  // Year-on-year % change, only where the denominator is not tiny (mirrors the
  // Shiny app's `Employees_lastyear > 4` rule) to avoid explosive percentages.
  r.pct = (r.employees != null && r.employeesLast != null && r.employeesLast >= 5)
    ? (r.employees - r.employeesLast) / r.employeesLast * 100
    : null;
  return r;
}

function buildSicIndex(rows) {
  const idx = {};
  for (const key of Object.keys(SIC_LEVELS)) {
    const { code, name } = SIC_LEVELS[key];
    const m = new Map();
    for (const r of rows) {
      const c = cleanCode(r[code]);
      let entry = m.get(c);
      if (!entry) { entry = { code: c, name: cleanCode(r[name]) ? String(r[name]).trim() : '', count: 0 }; m.set(c, entry); }
      entry.count++;
    }
    idx[key] = m;
  }
  return idx;
}

function buildSectionColors(sectionIndex) {
  const letters = [...sectionIndex.keys()].filter(Boolean).sort();
  const map = {};
  letters.forEach((l, i) => { map[l] = PALETTE[i % PALETTE.length]; });
  map[''] = '#9e9e9e';
  return map;
}

function quantileSorted(sortedArr, p) {
  if (!sortedArr.length) return 1;
  return sortedArr[Math.min(sortedArr.length - 1, Math.floor(p * sortedArr.length))];
}

function buildMeta(rows) {
  const emps = rows.map(r => r.employees).filter(v => v != null).sort((a, b) => a - b);
  const pcts = rows.map(r => (r.pct != null ? Math.abs(r.pct) : null)).filter(v => v != null).sort((a, b) => a - b);
  const maxAge = rows.reduce((m, r) => (r.age != null && r.age > m ? r.age : m), 0);
  return {
    maxAge: Math.ceil(maxAge),
    empRef: quantileSorted(emps, 0.98) || 1, // 98th pctile: keeps a few giant firms from flattening everything
    pctRef: quantileSorted(pcts, 0.98) || 1,
  };
}

export function loadData(url) {
  // PapaParse's worker runs from a blob: URL and resolves relative paths against
  // that blob base (which is invalid), so hand it an absolute URL.
  const absUrl = new URL(url, window.location.href).href;
  return new Promise((resolve, reject) => {
    Papa.parse(absUrl, {
      download: true,
      header: true,
      dynamicTyping: false,   // keep CompanyNumber and SIC codes as strings (leading zeros matter)
      skipEmptyLines: true,
      worker: true,           // parse off the main thread so the UI stays responsive
      complete: (res) => {
        try {
          const rows = res.data.filter(r => r && r.CompanyNumber).map(deriveRow);
          const sicIndex = buildSicIndex(rows);
          resolve({
            rows,
            headers: res.meta.fields,
            sicIndex,
            sectionColors: buildSectionColors(sicIndex.section),
            meta: buildMeta(rows),
          });
        } catch (e) { reject(e); }
      },
      error: (err) => reject(err),
    });
  });
}

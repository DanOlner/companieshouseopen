// export.js — write the current firm set to a downloadable CSV.
// Uses the global `Papa` (PapaParse) loaded via CDN in index.html.

function stamp() {
  const d = new Date(), p = (n) => String(n).padStart(2, '0');
  return `${d.getFullYear()}${p(d.getMonth() + 1)}${p(d.getDate())}_${p(d.getHours())}${p(d.getMinutes())}${p(d.getSeconds())}`;
}

function triggerDownload(blob, filename) {
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

// Keep every original column, append the computed pct_change.
export function exportCsv(rows, headers) {
  const cols = headers.includes('pct_change') ? headers.slice() : [...headers, 'pct_change'];
  const data = rows.map(r => cols.map(h => {
    if (h === 'pct_change') return r.pct == null ? '' : r.pct.toFixed(2);
    return r[h];
  }));
  const csv = Papa.unparse({ fields: cols, data });
  const blob = new Blob(['﻿' + csv], { type: 'text/csv;charset=utf-8;' }); // BOM for Excel
  triggerDownload(blob, `sy_firms_selection_${stamp()}.csv`);
}

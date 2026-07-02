# South Yorkshire Companies House firm map

A static, no-build web app (pure HTML/CSS/JS) that maps ~31.7k South Yorkshire
Companies House firms. A browser-only successor to the [SYMCA_shiny](https://github.com/DanOlner/SYMCA_shiny)
app — no R server needed.

## Features

- **Map** of every firm (Plotly `scattermapbox`, CartoDB Positron basemap), point
  size + colour by **employee count** (sequential) or **% employee change YoY**
  (diverging red→green, centred on 0). Toggle top-left.
- **Rectangle** and **lasso** selection (top bar → Box select / Lasso). The live
  "Selected" count updates as you drag.
- **Filters** (left sidebar): SIC sector at any level (Section / 2-/3-/5-digit),
  employee-count band, and firm age.
- **Export CSV** of the current selection — or, if nothing is selected, the firms
  currently shown. Keeps all original columns plus a computed `pct_change`.
- **SIC treemap** (bottom panel): section → 2-digit → 5-digit, sized by firm count
  or employees. It stays two-way in sync with the sidebar SIC filter: **click a tile
  to filter** the map + sidebar to that sector, **tick sidebar boxes** (one or many)
  to filter the treemap, and **zoom back out to "All South Yorkshire"** to clear the
  selection.
- **Website filter + links**: filter to firms with a validated website; in the
  popup, firms with a website have their name linked to it.
- **LA boundaries**: the 4 South Yorkshire local authorities are outlined beneath
  the points (`data/sy_localauthorities.geojson`, WGS84).
- Click any firm for a detail popup with a link to its Companies House page.

## Run locally

The app fetches a ~11.5 MB CSV, so it must be served over HTTP (not `file://`):

```bash
cd docs            # or cd docs/webmap
python3 -m http.server 8000
# then open http://localhost:8000/webmap/
```

## Data

`data/companieshouse_sy.csv` is a copy of
`local/companieshouse_sy_uptoJune2026_withgeo.csv` (the repo's `local/` is
gitignored, so the served copy lives here). To refresh, regenerate that CSV and
copy it over the top — no other change needed.

`% change` is derived in the browser as
`(Employees_thisyear − Employees_lastyear) / Employees_lastyear × 100`, and is only
shown for firms with **5+ employees last year** (mirrors the Shiny app, avoids
explosive small-denominator values).

## Publishing (GitHub Pages)

Lives under `docs/webmap/`, served by GitHub Pages (Pages source = `/docs`) at
`…/webmap/`. Self-contained, no build step; third-party libs (Plotly, PapaParse,
noUiSlider) load from CDNs.

The LA boundary GeoJSON was made from `SYMCA_shiny/data/mapdata/sy_localauthorityboundaries`
via R/sf: `st_transform(4326)` → `st_write(..., "GeoJSON")`.

## Possible next steps

- If the dataset grows much larger, swap PapaParse for DuckDB-WASM / Parquet.

## File map

| File | Role |
|------|------|
| `index.html` | Layout + CDN `<script>` tags |
| `js/data.js` | CSV load, derive `pct`, index the SIC hierarchy |
| `js/map.js` | Map trace, size/colour, drag-mode, selection + click events |
| `js/filters.js` | Sidebar controls + the filtering predicate |
| `js/treemap.js` | SIC treemap + click-to-filter |
| `js/export.js` | Selection → CSV download |
| `js/app.js` | Bootstrap + wiring |

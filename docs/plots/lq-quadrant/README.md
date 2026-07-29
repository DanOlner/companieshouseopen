# LQ quadrant plot

Interactive version of the quadrant plots in `ch_location_quotients.R`. Sector
concentration (location quotient) against direction of travel, for any of 350
local authorities, drillable from SIC section down to 5 digit.

## Running it locally

The page fetches its data, so `file://` won't work — you need a server:

```
cd docs/plots/lq-quadrant
python3 -m http.server 8000
# then http://localhost:8000
```

On GitHub Pages it just works.

## Regenerating the data

```
Rscript export_lq_for_web.R      # from the repo root
```

That rebuilds everything in `data/`. It runs the LQ pipeline over all five
levels and checks each one is an exact partition of the level below before
writing anything — if a check fails it stops rather than shipping a broken
drill-down.

Two things the export deliberately does not trust:

- **SIC sections are derived from the 2 digit code by range**, not taken from CH's
  `SIC_SECTION_LETTER`. That column is NULL for 126,774 firms which do have a
  valid numeric SIC, and codes 70, 74 and 98 carry two different values in it.
  Using it would leave section totals short of their own children.
- **Strings are forced to UTF-8.** SIC 22230's name arrives with a stray `0xC6`
  where an apostrophe belongs. `fetch().json()` rejects non-UTF-8 outright, so
  one bad byte would stop the whole page loading.

## Files

```
index.html        controls and panel containers
css/style.css
js/data.js        fetch, cache and shape the columnar JSON
js/quadrant.js    the plot: scales, marks, labels, interaction
js/controls.js    sidebar, place pickers, breadcrumb
js/app.js         state, and what needs loading before a redraw
data/             meta.json + one file per SIC level
```

`meta.json` and `lq_section.json` and `lq_sic2.json` load at startup (~1.2 MB);
the deeper levels are fetched the first time you drill or switch to them.
12.5 MB total on disk.

## Reading the plot

- **x** — location quotient, log scale. Right of the blue line the sector takes a
  bigger share of local employment than it does of GB employment.
- **y** — change in that LQ, or change in the raw employee count (toggle at the
  top). The scale is symlog, so gridlines are uneven: at 5 digit the range runs
  past +1000%, and on a linear axis that flattens everything else onto zero.
- **circle area** — employees.
- **big faint circle** — the sector you drilled into, one level up, so the parts
  can be read against the whole. Everything stays measured against GB at every
  level, which is what makes that comparison legible.

With two places showing, both panels share one set of axes and one drill state,
so vertical position is directly comparable and clicking a sector drills both.
Hovering rings the same sector in both. Where the other panel hasn't got it, it
says which of the two reasons applies — hidden by the current filters (with the
employee count, so you can see how near the threshold it was), or none recorded
in that place at all. Those mean quite different things and the plot shouldn't
leave you guessing.

## Caveats

Companies House records a registered address, not a workplace, so a multi-site
firm lands entirely at its registered office. Only firms reporting an employee
count in both years are counted (about 71%). Tiny sectors throw wild LQs — the
share filter in the sidebar is the sharper of the two controls for that.

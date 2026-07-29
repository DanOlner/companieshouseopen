/* The quadrant panel.
 *
 * x = LQ, log scale. y = % change, pseudo-log so one sector doubling doesn't
 * flatten everything else onto the zero line. Circle area = employees.
 *
 * Scales are handed in from app.js rather than computed here, so two stacked
 * panels can share identical ranges and be read against each other.
 */

window.Quadrant = (function () {

  var QUAD_COLOUR = {
    cg: '#1b7837',  // concentrated, growing
    cs: '#c0392b',  // concentrated, shrinking
    ug: '#5aa2d0',  // under-represented, growing
    us: '#9a9a9a'   // under-represented, shrinking
  };

  var M = { top: 16, right: 22, bottom: 46, left: 58 };

  function quadrantOf(d, metric) {
    var v = d[metric];
    if (d.lq >= 1) return v >= 0 ? 'cg' : 'cs';
    return v >= 0 ? 'ug' : 'us';
  }

  /* Symmetric log for the y axis: near-linear within +/-CONSTANT, logarithmic
   * beyond, and it handles negatives. Without it a single sector doubling its LQ
   * flattens every other sector onto the zero line.
   *
   * d3.scaleSymlog rather than a hand-rolled transform because d3-axis calls
   * scale.copy() internally - a plain function won't do.
   */
  var SYMLOG_CONSTANT = 5;

  var TICK_CANDIDATES = [-1000, -500, -200, -100, -50, -25, -10, -5, 0,
                         5, 10, 25, 50, 100, 200, 500, 1000, 2000];

  function yTicks(domain) {
    return TICK_CANDIDATES.filter(function (t) {
      return t >= domain[0] && t <= domain[1];
    });
  }

  function fmtPct(v) {
    if (v === null || v === undefined || !isFinite(v)) return 'n/a';
    return (v > 0 ? '+' : '') + d3.format('.1f')(v) + '%';
  }

  var fmtInt = d3.format(',d');

  /* Label placement: nudge overlapping labels vertically rather than running a
   * full force simulation. Enough for the 20-40 labels actually drawn.
   *
   * Nudging always pushes one way, so a crowded run walks off the end of the
   * panel - hence the clamp, and a reverse pass to clean up collisions the clamp
   * reintroduces at the boundary. Leftover overlap stays readable because the
   * labels carry a white halo.
   */
  function deconflict(items, lineHeight, minY, maxY) {
    items.sort(function (a, b) { return a.y - b.y; });

    for (var i = 1; i < items.length; i++) {
      if (items[i].y - items[i - 1].y < lineHeight) {
        items[i].y = items[i - 1].y + lineHeight;
      }
    }

    for (var j = items.length - 1; j >= 0; j--) {
      if (items[j].y > maxY) items[j].y = maxY;
      if (j < items.length - 1 && items[j + 1].y - items[j].y < lineHeight) {
        items[j].y = items[j + 1].y - lineHeight;
      }
      if (items[j].y < minY) items[j].y = minY;
    }

    return items;
  }

  /* opts:
   *   el          container element
   *   rows        rows to draw
   *   ghost       parent row drawn faintly behind, or null
   *   metric      'dlq' | 'djobs'
   *   x, y, r     shared scales
   *   width,height
   *   labelN      how many of the largest to label
   *   canDrill    whether clicking does anything
   *   onDrill(d)  click handler
   *   tip         tooltip element
   */
  function render(opts) {

    var el = d3.select(opts.el);
    var rows = opts.rows;
    var metric = opts.metric;
    var x = opts.x, y = opts.y, r = opts.r;
    var W = opts.width, H = opts.height;
    var innerW = W - M.left - M.right;
    var innerH = H - M.top - M.bottom;

    el.selectAll('svg').remove();

    var svg = el.append('svg')
      .attr('viewBox', '0 0 ' + W + ' ' + H)
      .attr('width', W)
      .attr('height', H);

    var g = svg.append('g').attr('transform', 'translate(' + M.left + ',' + M.top + ')');

    // ---- gridlines and axes ----

    var xt = x.ticks(6, '~g');
    var yt = yTicks(y.domain());

    g.append('g').selectAll('line.gx')
      .data(xt).enter().append('line')
      .attr('class', 'gridline')
      .attr('x1', x).attr('x2', x).attr('y1', 0).attr('y2', innerH);

    g.append('g').selectAll('line.gy')
      .data(yt).enter().append('line')
      .attr('class', 'gridline')
      .attr('x1', 0).attr('x2', innerW).attr('y1', y).attr('y2', y);

    g.append('g')
      .attr('class', 'axis')
      .attr('transform', 'translate(0,' + innerH + ')')
      .call(d3.axisBottom(x).tickValues(xt).tickFormat(d3.format('~g')));

    g.append('g')
      .attr('class', 'axis')
      .call(d3.axisLeft(y).tickValues(yt).tickFormat(function (d) { return d + '%'; }));

    // reference lines: LQ of 1, and no change
    g.append('line').attr('class', 'reflines')
      .attr('x1', x(1)).attr('x2', x(1)).attr('y1', 0).attr('y2', innerH);
    g.append('line').attr('class', 'reflines zero')
      .attr('x1', 0).attr('x2', innerW).attr('y1', y(0)).attr('y2', y(0));

    g.append('text')
      .attr('class', 'axislabel')
      .attr('x', innerW / 2).attr('y', innerH + 34)
      .attr('text-anchor', 'middle')
      .text('Location quotient, log scale — right of the blue line = more concentrated than GB');

    g.append('text')
      .attr('class', 'axislabel')
      .attr('transform', 'rotate(-90)')
      .attr('x', -innerH / 2).attr('y', -42)
      .attr('text-anchor', 'middle')
      .text(metric === 'dlq' ? '% change in LQ' : '% change in employees');

    // quadrant hints, tucked into the corners
    var pad = 6;
    g.append('text').attr('class', 'quadlabel')
      .attr('x', innerW - pad).attr('y', pad + 9).attr('text-anchor', 'end')
      .text('concentrated, growing');
    g.append('text').attr('class', 'quadlabel')
      .attr('x', innerW - pad).attr('y', innerH - pad).attr('text-anchor', 'end')
      .text('concentrated, shrinking');

    // ---- the parent, one level up, faint and behind ----

    if (opts.ghost && isFinite(opts.ghost[metric])) {
      var gh = opts.ghost;
      g.append('circle')
        .attr('class', 'ghost')
        .attr('cx', x(gh.lq)).attr('cy', y(gh[metric]))
        .attr('r', Math.max(r(gh.jobs), 14))
        .attr('fill', QUAD_COLOUR[quadrantOf(gh, metric)])
        .attr('fill-opacity', 0.10)
        .attr('stroke', QUAD_COLOUR[quadrantOf(gh, metric)])
        .attr('stroke-opacity', 0.45)
        .attr('stroke-dasharray', '4 3');

      // below the circle and truncated: the full name is already in the
      // breadcrumb, and at full length it collides with the point labels
      var ghr = Math.max(r(gh.jobs), 14);
      var ghName = gh.name.length > 34 ? gh.name.slice(0, 33) + '…' : gh.name;
      g.append('text')
        .attr('class', 'ghostlabel')
        .attr('x', x(gh.lq))
        .attr('y', y(gh[metric]) + ghr + 13)
        .attr('text-anchor', 'middle')
        .text('all of ' + ghName);
    }

    // ---- points ----

    var drawable = rows.filter(function (d) { return isFinite(d[metric]) && d.lq > 0; });

    // biggest first, so small circles stay clickable on top
    drawable.sort(function (a, b) { return b.jobs - a.jobs; });

    var pts = g.append('g').selectAll('circle.pt')
      .data(drawable, function (d) { return d.code; })
      .enter().append('circle')
      .attr('class', opts.canDrill ? 'pt' : 'pt nodrill')
      .attr('cx', function (d) { return x(d.lq); })
      .attr('cy', function (d) { return y(d[metric]); })
      .attr('r', function (d) { return r(d.jobs); })
      .attr('fill', function (d) { return QUAD_COLOUR[quadrantOf(d, metric)]; })
      .attr('fill-opacity', 0.78)
      .attr('stroke', '#fff')
      .attr('stroke-width', 0.7);

    // ---- labels for the largest ----

    var labelled = drawable.slice(0, opts.labelN);
    var lineHeight = 11;

    var labelItems = labelled.map(function (d) {
      return { d: d, x: x(d.lq), y: y(d[metric]), r: r(d.jobs) };
    });

    // split above and below the midline so nudging doesn't pile everything down
    var mid = innerH / 2;
    var above = deconflict(labelItems.filter(function (i) { return i.y <= mid; }),
                           lineHeight, 9, innerH - 4);
    var below = deconflict(labelItems.filter(function (i) { return i.y > mid; }),
                           lineHeight, 9, innerH - 4);

    var labelG = g.append('g');

    labelG.selectAll('line.leader')
      .data(above.concat(below)).enter().append('line')
      .attr('class', 'leader')
      .attr('x1', function (i) { return i.x; })
      .attr('y1', function (i) { return y(i.d[metric]); })
      .attr('x2', function (i) { return i.x + i.r + 3; })
      .attr('y2', function (i) { return i.y - 3; });

    labelG.selectAll('text.ptlabel')
      .data(above.concat(below)).enter().append('text')
      .attr('class', 'ptlabel')
      .attr('x', function (i) { return i.x + i.r + 4; })
      .attr('y', function (i) { return i.y - 2; })
      .text(function (i) {
        var n = i.d.name;
        return n.length > 46 ? n.slice(0, 45) + '…' : n;
      })
      .each(function (i) {
        // flip to the left if the label would run off the right edge
        var w = this.getComputedTextLength();
        if (i.x + i.r + 6 + w > innerW) {
          d3.select(this).attr('x', i.x - i.r - 4).attr('text-anchor', 'end');
          labelG.selectAll('line.leader').filter(function (j) { return j === i; })
            .attr('x2', i.x - i.r - 3);
        }
      });

    // ---- cross-panel highlight ----
    //
    // Hovering a sector in one panel marks the same sector in the other, so the
    // two places can be read against each other. The sector may well be missing
    // from the other panel - filtered out, or simply not present - so say which
    // rather than leaving a silent gap.

    var byCode = new Map(drawable.map(function (d) { return [d.code, d]; }));
    var hoverLayer = g.append('g').attr('class', 'hoverlayer');
    var labelledCodes = new Set(labelled.map(function (d) { return d.code; }));

    function highlight(code, missingReason) {
      hoverLayer.selectAll('*').remove();
      pts.attr('stroke', '#fff').attr('stroke-width', 0.7);

      if (!code) return;

      var d = byCode.get(code);

      if (!d) {
        // top left: the only corner without a quadrant label in it
        hoverLayer.append('text')
          .attr('class', 'missingnote')
          .attr('x', 4)
          .attr('y', 12)
          .attr('text-anchor', 'start')
          .text('this sector: ' + (missingReason || 'not shown here'));
        return;
      }

      var px = x(d.lq), py = y(d[metric]), pr = r(d.jobs);

      pts.filter(function (p) { return p.code === code; })
        .attr('stroke', '#000').attr('stroke-width', 2).raise();

      hoverLayer.append('circle')
        .attr('class', 'hoverring')
        .attr('cx', px).attr('cy', py)
        .attr('r', pr + 5);

      // only add a label if this point isn't already carrying one
      if (!labelledCodes.has(code)) {
        var t = hoverLayer.append('text')
          .attr('class', 'ptlabel hover')
          .attr('x', px + pr + 7)
          .attr('y', py - 4)
          .text(d.name);
        if (px + pr + 9 + t.node().getComputedTextLength() > innerW) {
          t.attr('x', px - pr - 7).attr('text-anchor', 'end');
        }
      }
    }

    // ---- interaction ----

    var tip = d3.select(opts.tip);

    pts
      .on('mousemove', function (event, d) {
        if (opts.onHover) opts.onHover(d.code);
        var drillNote = opts.canDrill
          ? '<span class="tipdrill">click to drill in →</span>' : '';
        tip.html(
          '<span class="tipname">' + d.name + '</span>' +
          '<span class="tiprow">LQ <b>' + d3.format('.2f')(d.lq) + '</b> &middot; ' +
          'employees <b>' + fmtInt(d.jobs) + '</b> (' +
          d3.format('.2f')(d.share * 100) + '% of the place)</span><br>' +
          '<span class="tiprow">LQ change <b>' + fmtPct(d.dlq) + '</b> &middot; ' +
          'employee change <b>' + fmtPct(d.djobs) + '</b></span>' +
          drillNote
        )
          .style('left', (event.pageX + 14) + 'px')
          .style('top', (event.pageY - 12) + 'px')
          .classed('on', true);
      })
      .on('mouseleave', function () {
        tip.classed('on', false);
        if (opts.onHover) opts.onHover(null);
      })
      .on('click', function (event, d) {
        if (!opts.canDrill || !opts.onDrill) return;
        // the point is about to stop existing, so drop its tooltip
        tip.classed('on', false);
        opts.onDrill(d);
      });

    return { svg: svg, highlight: highlight, hasCode: function (c) { return byCode.has(c); } };
  }

  /* Shared scales across whatever rows the panels will draw, so stacked panels
   * are directly comparable. */
  function makeScales(allRows, ghosts, metric, width, height) {
    var innerW = width - M.left - M.right;
    var innerH = height - M.top - M.bottom;

    var usable = allRows.filter(function (d) { return isFinite(d[metric]) && d.lq > 0; });
    ghosts.forEach(function (g) {
      if (g && isFinite(g[metric]) && g.lq > 0) usable.push(g);
    });

    if (!usable.length) usable = [{ lq: 1, jobs: 1, dlq: 0, djobs: 0 }];

    var lqExtent = d3.extent(usable, function (d) { return d.lq; });
    // always keep LQ 1 in frame - it is the whole reference
    var lo = Math.min(lqExtent[0], 0.9) * 0.85;
    var hi = Math.max(lqExtent[1], 1.1) * 1.18;

    // deliberately no .nice(): on a log scale it rounds out to whole powers of
    // ten, so data spanning 0.5-20 would give an axis running 0.1-100 and waste
    // half the panel
    var x = d3.scaleLog().domain([lo, hi]).range([0, innerW]);

    var vExtent = d3.extent(usable, function (d) { return d[metric]; });
    var vlo = Math.min(vExtent[0], -1);
    var vhi = Math.max(vExtent[1], 1);
    var padv = (vhi - vlo) * 0.06;

    var y = d3.scaleSymlog()
      .constant(SYMLOG_CONSTANT)
      .domain([vlo - padv, vhi + padv])
      .range([innerH, 0]);

    var maxJobs = d3.max(usable, function (d) { return d.jobs; }) || 1;
    var r = d3.scaleSqrt().domain([0, maxJobs]).range([2, 26]);

    return { x: x, y: y, r: r, margin: M };
  }

  return { render: render, makeScales: makeScales, colours: QUAD_COLOUR };

})();

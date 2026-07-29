/* Orchestration: owns the state, decides what needs loading, redraws.
 *
 * Both panels always share drill state and scales, so the two places are read
 * against each other rather than being two plots that happen to be stacked.
 */

(function () {

  var state = {
    level: 'section',
    parentCode: null,
    trail: [],            // [{level, code, name}] - the drill path
    showAll: false,
    metric: 'dlq',
    placeA: 0,
    placeB: 0,
    showB: false,
    minJobs: 100,
    minShare: 0,
    labelN: 20
  };

  var els = {};
  var countA = null, countB = null;
  var tip = null;

  // live handles on the two rendered panels, for cross-panel highlighting
  var panels = { A: null, B: null };
  var hoverCode = null;

  // ---- lifecycle ----

  function init() {
    els.status = document.getElementById('status');
    els.breadcrumb = document.getElementById('breadcrumb');
    els.panelA = document.getElementById('panelA');
    els.panelB = document.getElementById('panelB');
    els.levelPicker = document.getElementById('levelPicker');

    tip = document.createElement('div');
    tip.id = 'tip';
    document.body.appendChild(tip);

    LQData.loadMeta()
      .then(function (meta) {
        // sensible openers if they exist in the data
        state.placeA = pickDefault(meta, ['Sheffield'], 0);
        state.placeB = pickDefault(meta, ['Leeds'], 1);

        Controls.buildLevelPicker(els.levelPicker, meta, state.level, onPickLevel);
        countA = Controls.buildPanelHead(els.panelA, meta, state.placeA, function (i) {
          state.placeA = i; redraw();
        });
        countB = Controls.buildPanelHead(els.panelB, meta, state.placeB, function (i) {
          state.placeB = i; redraw();
        });

        Controls.wireSliders({
          onMinJobs: function (v) { state.minJobs = v; redraw(); },
          onMinShare: function (v) { state.minShare = v; redraw(); },
          onLabelN: function (v) { state.labelN = v; redraw(); }
        });

        Controls.wireToggles({
          onMetric: function (m) { state.metric = m; redraw(); },
          onShowAll: function (v) { state.showAll = v; redraw(); },
          onTogglePanelB: function (on) {
            state.showB = on;
            els.panelB.classList.toggle('hidden', !on);
            redraw();
          }
        });

        window.addEventListener('resize', debounce(redraw, 180));

        return ensureLoaded();
      })
      .then(redraw)
      .catch(fail);
  }

  function pickDefault(meta, names, fallback) {
    for (var i = 0; i < names.length; i++) {
      if (meta.placeIndex.has(names[i])) return meta.placeIndex.get(names[i]);
    }
    return fallback;
  }

  /* The current level, plus the parent level so the faint parent circle can be
   * drawn. Both are cached after the first fetch. */
  function ensureLoaded() {
    var needed = [state.level];
    var up = LQData.parentLevelOf(state.level);
    if (up) needed.push(up);

    var missing = needed.filter(function (l) { return !LQData.isLoaded(l); });
    if (!missing.length) return Promise.resolve();

    setStatus('Loading ' + missing.join(' and ') + '…');
    return Promise.all(missing.map(LQData.loadLevel)).then(function () { setStatus(''); });
  }

  function fail(e) {
    setStatus('Could not load data: ' + e.message);
    console.error(e);
  }

  function setStatus(msg) {
    els.status.textContent = msg || '';
    els.status.style.display = msg ? '' : 'none';
  }

  // ---- state transitions ----

  function onPickLevel(lv) {
    // jumping straight to a level abandons the drill path
    state.level = lv;
    state.trail = [];
    state.parentCode = null;
    Controls.setLevelPicker(els.levelPicker, lv);
    ensureLoaded().then(redraw).catch(fail);
  }

  function onDrill(d) {
    var child = LQData.childLevelOf(state.level);
    if (!child) return;
    state.trail.push({ level: state.level, code: d.code, name: d.name });
    state.level = child;
    state.parentCode = d.code;
    Controls.setLevelPicker(els.levelPicker, child);
    ensureLoaded().then(redraw).catch(fail);
  }

  function onCrumb(depth) {
    state.trail = state.trail.slice(0, depth);
    if (!state.trail.length) {
      state.level = 'section';
      state.parentCode = null;
    } else {
      var last = state.trail[state.trail.length - 1];
      state.level = LQData.childLevelOf(last.level);
      state.parentCode = last.code;
    }
    Controls.setLevelPicker(els.levelPicker, state.level);
    ensureLoaded().then(redraw).catch(fail);
  }

  // ---- drawing ----

  function redraw() {
    var meta = LQData.getMeta();
    if (!meta || !LQData.isLoaded(state.level)) return;

    Controls.renderBreadcrumb(els.breadcrumb, state.trail, onCrumb);

    // showAll ignores the drill filter but keeps the breadcrumb, so you can go back
    var parentCode = state.showAll ? null : state.parentCode;

    var opts = {
      parentCode: parentCode,
      minJobs: state.minJobs,
      minShare: state.minShare
    };

    var rowsA = LQData.forPlace(state.level, state.placeA, opts);
    var rowsB = state.showB ? LQData.forPlace(state.level, state.placeB, opts) : [];

    var ghostA = LQData.parentRow(state.level, state.placeA, parentCode);
    var ghostB = state.showB ? LQData.parentRow(state.level, state.placeB, parentCode) : null;

    var width = Math.max(els.panelA.clientWidth || 900, 420);
    var height = state.showB ? 430 : 560;

    // one set of scales across both panels, so vertical position compares
    var scales = Quadrant.makeScales(
      rowsA.concat(rowsB), [ghostA, ghostB], state.metric, width, height
    );

    var canDrill = !!LQData.childLevelOf(state.level) && !state.showAll;

    // the old panels are about to be torn down, so drop any stale hover
    hoverCode = null;

    panels.A = drawPanel(els.panelA, rowsA, ghostA, scales, width, height, canDrill, countA);
    panels.B = state.showB
      ? drawPanel(els.panelB, rowsB, ghostB, scales, width, height, canDrill, countB)
      : null;

    if (!rowsA.length && (!state.showB || !rowsB.length)) {
      setStatus('Nothing left after the filters — try lowering the minimum employees or share.');
    } else {
      setStatus('');
    }
  }

  function drawPanel(el, rows, ghost, scales, width, height, canDrill, countEl) {
    // clear the old svg but keep the header
    Array.prototype.forEach.call(el.querySelectorAll('svg'), function (s) { s.remove(); });

    var host = el.querySelector('.plothost');
    if (!host) {
      host = document.createElement('div');
      host.className = 'plothost';
      el.appendChild(host);
    }

    var api = Quadrant.render({
      el: host,
      rows: rows,
      ghost: ghost,
      metric: state.metric,
      x: scales.x, y: scales.y, r: scales.r,
      width: width, height: height,
      labelN: state.labelN,
      canDrill: canDrill,
      onDrill: onDrill,
      onHover: onHover,
      tip: tip
    });

    if (countEl) {
      var drawn = rows.filter(function (d) { return isFinite(d[state.metric]) && d.lq > 0; }).length;
      var jobs = rows.reduce(function (a, d) { return a + d.jobs; }, 0);
      countEl.textContent = drawn + ' sectors shown · ' +
        jobs.toLocaleString() + ' employees';
    }

    return api;
  }

  /* Hovering a sector in either panel marks it in both.
   *
   * It may not be in the other panel at all. Two different reasons, and they
   * mean different things, so work out which: the sector can be absent from the
   * data for that place entirely, or present but cut by the current filters. */
  function onHover(code) {
    if (code === hoverCode) return;   // mousemove fires constantly
    hoverCode = code;

    ['A', 'B'].forEach(function (k) {
      var panel = panels[k];
      if (!panel) return;
      if (!code || panel.hasCode(code)) {
        panel.highlight(code);
        return;
      }
      panel.highlight(code, missingReason(code, k === 'A' ? state.placeA : state.placeB));
    });
  }

  function missingReason(code, placeIdx) {
    // re-query with the filters off: if it shows up now, the filters hid it
    var unfiltered = LQData.forPlace(state.level, placeIdx, {
      parentCode: state.showAll ? null : state.parentCode,
      minJobs: 0,
      minShare: 0
    });

    for (var i = 0; i < unfiltered.length; i++) {
      if (unfiltered[i].code === code) {
        return 'hidden by the filters here (' +
          unfiltered[i].jobs.toLocaleString() + ' employees)';
      }
    }
    return 'none recorded here';
  }

  function debounce(fn, ms) {
    var t;
    return function () {
      clearTimeout(t);
      t = setTimeout(fn, ms);
    };
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

})();

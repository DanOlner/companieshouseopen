/* Sidebar controls, place pickers and breadcrumb.
 * Pure DOM wiring: everything calls back into app.js, which owns the state. */

window.Controls = (function () {

  function buildLevelPicker(el, meta, current, onPick) {
    el.innerHTML = '';
    meta.levels.forEach(function (lv) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'seg' + (lv === current ? ' active' : '');
      b.dataset.level = lv;
      b.textContent = meta.levelLabels[lv] || lv;
      b.addEventListener('click', function () { onPick(lv); });
      el.appendChild(b);
    });
  }

  function setLevelPicker(el, current) {
    Array.prototype.forEach.call(el.querySelectorAll('.seg'), function (b) {
      b.classList.toggle('active', b.dataset.level === current);
    });
  }

  function buildPlaceSelect(meta, selectedIdx, onChange) {
    var sel = document.createElement('select');
    meta.places.forEach(function (p, i) {
      var o = document.createElement('option');
      o.value = String(i);
      o.textContent = p;
      if (i === selectedIdx) o.selected = true;
      sel.appendChild(o);
    });
    sel.addEventListener('change', function () { onChange(+sel.value); });
    return sel;
  }

  /* Panel header: place picker plus a count of what is currently drawn */
  function buildPanelHead(panelEl, meta, placeIdx, onChange) {
    var head = panelEl.querySelector('.panelhead');
    if (!head) {
      head = document.createElement('div');
      head.className = 'panelhead';
      panelEl.insertBefore(head, panelEl.firstChild);
    }
    head.innerHTML = '';
    head.appendChild(buildPlaceSelect(meta, placeIdx, onChange));
    var count = document.createElement('span');
    count.className = 'panelcount';
    head.appendChild(count);
    return count;
  }

  /* All › C Manufacturing › 25 Fabricated metal
   * Clicking a crumb climbs back to that depth. */
  function renderBreadcrumb(el, trail, onCrumb) {
    el.innerHTML = '';

    function crumb(label, idx, isCurrent) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'crumb' + (isCurrent ? ' current' : '');
      b.textContent = label;
      if (!isCurrent) b.addEventListener('click', function () { onCrumb(idx); });
      el.appendChild(b);
    }

    crumb('All sectors', 0, trail.length === 0);

    trail.forEach(function (t, i) {
      var sep = document.createElement('span');
      sep.className = 'crumbsep';
      sep.textContent = '›';
      el.appendChild(sep);
      crumb(t.name, i + 1, i === trail.length - 1);
    });
  }

  function wireSliders(handlers) {
    var minJobs = document.getElementById('minJobs');
    var minJobsVal = document.getElementById('minJobsVal');
    var minShare = document.getElementById('minShare');
    var minShareVal = document.getElementById('minShareVal');
    var labelN = document.getElementById('labelN');
    var labelNVal = document.getElementById('labelNVal');

    minJobs.addEventListener('input', function () {
      minJobsVal.textContent = minJobs.value;
      handlers.onMinJobs(+minJobs.value);
    });

    // slider is in hundredths of a percent, so 0-200 covers 0-2%
    minShare.addEventListener('input', function () {
      var pct = (+minShare.value) / 100;
      minShareVal.textContent = pct.toFixed(2) + '%';
      handlers.onMinShare(pct / 100);
    });

    labelN.addEventListener('input', function () {
      labelNVal.textContent = labelN.value;
      handlers.onLabelN(+labelN.value);
    });
  }

  function wireToggles(handlers) {
    var metricToggle = document.getElementById('metricToggle');
    metricToggle.addEventListener('click', function (e) {
      var b = e.target.closest('.seg');
      if (!b) return;
      Array.prototype.forEach.call(metricToggle.querySelectorAll('.seg'), function (s) {
        s.classList.toggle('active', s === b);
      });
      handlers.onMetric(b.dataset.metric);
    });

    document.getElementById('showAll').addEventListener('change', function (e) {
      handlers.onShowAll(e.target.checked);
    });

    var toggleB = document.getElementById('togglePanelB');
    toggleB.addEventListener('click', function () {
      var on = toggleB.getAttribute('aria-pressed') !== 'true';
      toggleB.setAttribute('aria-pressed', String(on));
      toggleB.textContent = on ? '− second place' : '+ second place';
      handlers.onTogglePanelB(on);
    });

    var modal = document.getElementById('infoModal');
    document.getElementById('infoBtn').addEventListener('click', function () {
      modal.classList.remove('hidden');
    });
    document.getElementById('infoClose').addEventListener('click', function () {
      modal.classList.add('hidden');
    });
    modal.addEventListener('click', function (e) {
      if (e.target === modal) modal.classList.add('hidden');
    });
  }

  return {
    buildLevelPicker: buildLevelPicker,
    setLevelPicker: setLevelPicker,
    buildPanelHead: buildPanelHead,
    renderBreadcrumb: renderBreadcrumb,
    wireSliders: wireSliders,
    wireToggles: wireToggles
  };

})();

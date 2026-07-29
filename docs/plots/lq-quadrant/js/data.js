/* Data loading and shaping.
 *
 * The export writes one columnar file per SIC level, all 350 local authorities in
 * each. Sections and 2 digit load at startup; deeper levels are fetched the first
 * time someone drills or switches to them, and cached thereafter.
 */

window.LQData = (function () {

  var meta = null;
  var levelCache = {};   // level -> {rows: [...], byPlace: Map(placeIdx -> [rows])}
  var pending = {};      // level -> Promise, so two callers don't double-fetch

  function loadMeta() {
    return fetch('data/meta.json')
      .then(function (r) {
        if (!r.ok) throw new Error('meta.json: ' + r.status);
        return r.json();
      })
      .then(function (m) {
        meta = m;
        // place name -> index, for the pickers
        meta.placeIndex = new Map(meta.places.map(function (p, i) { return [p, i]; }));
        return m;
      });
  }

  /* Columnar arrays -> row objects, indexed by place.
   * Done once per level; every later render just filters the cached arrays. */
  function shape(level, raw) {
    var codes = meta.sectors[level].codes;
    var names = meta.sectors[level].names;
    var rows = new Array(raw.n);
    var byPlace = new Map();

    for (var i = 0; i < raw.n; i++) {
      var row = {
        p: raw.p[i],
        s: raw.s[i],
        code: codes[raw.s[i]],
        name: names[raw.s[i]],
        lq: raw.lq[i],
        dlq: raw.dlq[i],
        djobs: raw.djobs[i],
        jobs: raw.jobs[i],
        share: raw.share[i]
      };
      rows[i] = row;
      var bucket = byPlace.get(row.p);
      if (bucket) bucket.push(row); else byPlace.set(row.p, [row]);
    }

    return { rows: rows, byPlace: byPlace };
  }

  function loadLevel(level) {
    if (levelCache[level]) return Promise.resolve(levelCache[level]);
    if (pending[level]) return pending[level];

    pending[level] = fetch('data/lq_' + level + '.json')
      .then(function (r) {
        if (!r.ok) throw new Error('lq_' + level + '.json: ' + r.status);
        return r.json();
      })
      .then(function (raw) {
        levelCache[level] = shape(level, raw);
        delete pending[level];
        return levelCache[level];
      })
      .catch(function (e) {
        delete pending[level];
        throw e;
      });

    return pending[level];
  }

  function isLoaded(level) { return !!levelCache[level]; }

  /* Rows for one place at one level, already filtered.
   *
   * parentCode limits to the children of whatever has been drilled into:
   *   at sic2, the parent is a section letter, so we go via the lookup
   *   deeper down, a child's code simply starts with its parent's
   */
  function forPlace(level, placeIdx, opts) {
    var cache = levelCache[level];
    if (!cache) return [];

    var rows = cache.byPlace.get(placeIdx) || [];
    var parentCode = opts.parentCode;
    var minJobs = opts.minJobs || 0;
    var minShare = opts.minShare || 0;

    return rows.filter(function (d) {
      if (d.jobs < minJobs) return false;
      if (d.share < minShare) return false;
      if (!parentCode) return true;
      if (level === 'sic2') return meta.sic2ToSection[d.code] === parentCode;
      return d.code.indexOf(parentCode) === 0;
    });
  }

  /* The parent's own row, one level up, so it can be drawn faintly behind its
   * children. Null at the top level or when showing everything. */
  function parentRow(level, placeIdx, parentCode) {
    if (!parentCode) return null;
    var up = parentLevelOf(level);
    if (!up) return null;
    var cache = levelCache[up];
    if (!cache) return null;
    var rows = cache.byPlace.get(placeIdx) || [];
    for (var i = 0; i < rows.length; i++) {
      if (rows[i].code === parentCode) return rows[i];
    }
    return null;
  }

  function parentLevelOf(level) {
    var i = meta.levels.indexOf(level);
    return i > 0 ? meta.levels[i - 1] : null;
  }

  function childLevelOf(level) {
    var i = meta.levels.indexOf(level);
    return (i > -1 && i < meta.levels.length - 1) ? meta.levels[i + 1] : null;
  }

  return {
    loadMeta: loadMeta,
    loadLevel: loadLevel,
    isLoaded: isLoaded,
    forPlace: forPlace,
    parentRow: parentRow,
    parentLevelOf: parentLevelOf,
    childLevelOf: childLevelOf,
    getMeta: function () { return meta; }
  };

})();

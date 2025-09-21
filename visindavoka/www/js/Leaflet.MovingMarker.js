/* Note!! this was written by ChatGPT
 * 
 * Lightweight MovingMarker for Leaflet
 * Implements the common API:
 *   L.Marker.movingMarker(latlngs, durations, options)
 *   L.movingMarker(latlngs, durations, options) // alias
 * Features:
 *   - Per-segment durations (ms) in `durations` (length = latlngs.length - 1)
 *   - requestAnimationFrame-based interpolation (linear)
 *   - options.autostart => start automatically when added
 *   - Works with any L.Icon or L.DivIcon (e.g., an emoji duck)
 * This is a minimal, dependency-free implementation.
 */

(function (factory) {
  if (typeof define === 'function' && define.amd) {
    define(['leaflet'], factory);
  } else if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = factory(require('leaflet'));
  } else {
    factory(L);
  }
}(function (L) {
  if (!L) { throw new Error('Leaflet is required for MovingMarker'); }

  const clamp = (v, a, b) => Math.max(a, Math.min(b, v));

  const MovingMarker = L.Marker.extend({
    initialize: function (latlngs, durations, options) {
      if (!Array.isArray(latlngs) || latlngs.length < 2) {
        throw new Error('movingMarker: need at least two latlngs');
      }
      if (!Array.isArray(durations) || durations.length !== (latlngs.length - 1)) {
        throw new Error('movingMarker: durations must have length latlngs.length - 1');
      }
      this._latlngs = latlngs.map(function (ll) { return L.latLng(ll); });
      this._durations = durations.slice(0);
      this._i = 0;
      this._running = false;
      this._t0 = 0;
      this._raf = null;
      options = options || {};
      L.Marker.prototype.initialize.call(this, this._latlngs[0], options);
      if (options.autostart) {
        // defer start until layer is on the map
        this.once('add', this.start, this);
      }
    },

    onAdd: function (map) {
      L.Marker.prototype.onAdd.call(this, map);
      // if autostart was requested but 'add' happened before initialize hook
      if (this.options && this.options._deferredAutostart) {
        this.options._deferredAutostart = false;
        this.start();
      }
    },

    start: function () {
      if (this._running) return this;
      this._running = true;
      this._i = clamp(this._i, 0, this._latlngs.length - 2);
      this._t0 = performance.now();
      this._from = this._latlngs[this._i];
      this._to   = this._latlngs[this._i + 1];
      this._segDur = Math.max(1, this._durations[this._i]); // ms
      this._step = this._step.bind(this);
      this._raf = requestAnimationFrame(this._step);
      this.fire('start');
      return this;
    },

    pause: function () {
      if (!this._running) return this;
      this._running = false;
      if (this._raf) cancelAnimationFrame(this._raf);
      this._raf = null;
      this.fire('pause');
      return this;
    },

    stop: function () {
      if (this._raf) cancelAnimationFrame(this._raf);
      this._raf = null;
      this._running = false;
      this._i = 0;
      this.setLatLng(this._latlngs[0]);
      this.fire('stop');
      return this;
    },

    isRunning: function () { return !!this._running; },

    _step: function (t) {
      if (!this._running) return;

      const elapsed = t - this._t0;
      let p = clamp(elapsed / this._segDur, 0, 1);

      // linear interpolation in lat/lng
      const lat = this._from.lat + (this._to.lat - this._from.lat) * p;
      const lng = this._from.lng + (this._to.lng - this._from.lng) * p;
      this.setLatLng([lat, lng]);
      this.fire('move', { latlng: this.getLatLng() });

      if (p >= 1) {
        // move to next segment
        this._i += 1;
        if (this._i >= this._latlngs.length - 1) {
          this._running = false;
          this.fire('end');
          return;
        }
        this._from = this._latlngs[this._i];
        this._to   = this._latlngs[this._i + 1];
        this._segDur = Math.max(1, this._durations[this._i]);
        this._t0 = t;
      }
      this._raf = requestAnimationFrame(this._step);
    }
  });

  // Factory and aliases
  L.Marker.MovingMarker = MovingMarker;
  L.Marker.movingMarker = function (latlngs, durations, options) {
    const mm = new MovingMarker(latlngs, durations, options || {});
    // If autostart specified but layer not yet on map, defer
    if (mm.options && mm.options.autostart && !mm._map) {
      mm.options._deferredAutostart = true;
    }
    return mm;
  };
  L.movingMarker = function (latlngs, durations, options) {
    return L.Marker.movingMarker(latlngs, durations, options);
  };

  return MovingMarker;
}));

/* Attempting to use
 * https://developers.google.com/closure/compiler/docs/externs-and-exports to
 * force Closure compiler to recognize triangles/halfedges as properties on
 * Delaunator after advanced compilation.
 */
var Delaunator = class {
  constructor() {
    /** @type {Float64Array} */
    this.coords;
    /** @type {Number} */
    this.trianglesLen;
    /** @type {Uint32Array} */
    this.triangles;
    /** @type {Uint32Array} */
    this.halfedges;
    /** @type {Uint32Array} */
    this.hull;
  }
};

/** @return {Delaunator} */
Delaunator.from = function () {};

Delaunator.prototype = {
  "coords": function() {},
  "triangles": function() {},
  "trianglesLen": function() {},
  "halfedges": function() {},
  "hull": function() {},

  "update": function() {},
  "_addTriangle": function () {},
  "_hashKey": function () {},
  "_legalize": function () {},
  "_link": function () {}
};

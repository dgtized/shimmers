/* Attempting to use
 * https://developers.google.com/closure/compiler/docs/externs-and-exports to
 * force Closure compiler to recognize triangles/halfedges as properties on
 * Delaunator after advanced compilation.
 */
var Delaunator = class {
  constructor() {
    this.coords;
    /** @type {Uint32Array} */
    this._triangles;
    /** @type {Uint32Array} */
    this._halfedges;
  }

  from() {}
};

Delaunator.prototype = {
  "update": function() {},
  "_addTriangle": function () {},
  "_hashKey": function () {},
  "_legalize": function () {},
  "_link": function () {}
};

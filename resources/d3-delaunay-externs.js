var Delaunay = class {
  constructor() {
    this.inedges;
    this.points;
    this.triangles;
    this.halfedges;
    this.hull;
  }
};
/**
 * @return {Delaunay}
 */
Delaunay.from = function() {};
Delaunay.prototype = {
  "update": function() {},
  "voronoi": function() {},
  "neighbors": function() {},
  "find": function() {},
  "render": function() {},
  "renderPoints": function() {},
  "renderHull": function() {},
  "hullPolygon": function() {},
  "renderTriangle": function() {},
  "trianglePolygons": function() {},
  "trianglePolygon": function() {},
};
/**
 * @return {Voronoi}
 */
Delaunay.prototype.voronoi = function() {};

var Path = class {
  constructor() {}
};
Path.prototype = {
  "moveTo": function() {},
  "closePath": function() {},
  "lineTo": function() {},
  "arc": function() {},
  "rect": function() {},
  "value": function() {},
};

var Polygon = class {
  constructor() {}
};
Polygon.prototype = {
  "moveTo": function() {},
  "closePath": function() {},
  "lineTo": function() {},
  "value": function() {},
};

var Voronoi = class {
  constructor() {
    this.delaunay;
    this.vectors;
    this.circumcenters;
    this.xmax;
    this.ymax;
  }
};
Voronoi.prototype = {
  "update": function() {},
  "render": function() {},
  "renderBounds": function() {},
  "renderCell": function() {},
  "cellPolygons": function() {},
  "cellPolygon": function() {},
  "contains": function() {},
  "neighbors": function() {},
};

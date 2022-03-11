var Delaunay = class {
  constructor() {
    this.inedges;
    this.points;
    this.triangles;
    this.halfedges;
    this.hull;
  }

  update() {}
  /** @return {Voronoi} */
  voronoi() {}
  neighbors() {}
  find() {}
  render() {}
  renderPoints() {}
  renderHull() {}
  hullPolygon() {}
  renderTriangles() {}
  trianglePolygons() {}
  trianglePolygon() {}
};
/** @return {Delaunay} */
Delaunay.from = function() {};

var Path = class {
  constructor() {}

  moveTo() {}
  closePath() {}
  lineTo() {}
  arc() {}
  rect() {}
  value() {}
};

var Polygon = class {
  constructor() {}

  moveTo() {}
  closePath() {}
  lineTo() {}
  value() {}
};

var Voronoi = class {
  constructor() {
    this.delaunay;
    this.vectors;
    this.circumcenters;
    this.xmax;
    this.ymax;
  }

  update() {}
  render() {}
  renderBounds() {}
  renderCell() {}
  cellPolygons() {}
  cellPolygon() {}
  contains() {}
  neighbors() {}
};

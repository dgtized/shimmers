(ns shimmers.common.quil-draws-geom
  (:require #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Rect2 Triangle2 Polygon2]])
            [shimmers.common.quil :as cq]
            [quil.core :as q]
            [thi.ng.geom.core :as geom]))

(defprotocol QuilDrawGeom
  (draw [s]))

(extend-type thi.ng.geom.types.Rect2
  QuilDrawGeom
  (draw [s]
    (cq/rectangle s)))

(extend-type thi.ng.geom.types.Triangle2
  QuilDrawGeom
  (draw [s]
    (apply cq/draw-triangle (:points s))))

(extend-type thi.ng.geom.types.Polygon2
  QuilDrawGeom
  (draw [s]
    (q/begin-shape)
    (doseq [v (geom/vertices s)]
      (apply q/vertex v))
    (q/end-shape :close)))


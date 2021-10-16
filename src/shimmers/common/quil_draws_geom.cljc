(ns shimmers.common.quil-draws-geom
  (:require #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Rect2 Triangle2]])
            [shimmers.common.quil :as cq]))

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


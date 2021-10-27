(ns shimmers.common.quil-draws-geom
  #?@
  (:clj
   [(:require [shimmers.common.quil :as cq])
    (:import
     shimmers.math.geometry.group.Group
     [thi.ng.geom.types Circle2 Polygon2 Rect2 Triangle2])]
   :cljs
   [(:require
     [shimmers.common.quil :as cq]
     [shimmers.math.geometry.group :refer [Group]]
     [thi.ng.geom.types :refer [Circle2 Polygon2 Rect2 Triangle2]])]))

(defprotocol QuilDrawGeom
  (draw [s]))

;; Requires (q/ellipse-mode :radius) to render as expected
(extend-type Circle2
  QuilDrawGeom
  (draw [s] (cq/circle s)))

(extend-type Rect2
  QuilDrawGeom
  (draw [s] (cq/rectangle s)))

(extend-type Triangle2
  QuilDrawGeom
  (draw [s]
    (apply cq/draw-triangle (:points s))))

(extend-type Polygon2
  QuilDrawGeom
  (draw [s] (cq/draw-polygon s)))

(extend-type Group
  QuilDrawGeom
  (draw [group]
    (doseq [s (:children group)]
      (draw s))))

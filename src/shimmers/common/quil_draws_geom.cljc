(ns shimmers.common.quil-draws-geom
  #?@
  (:clj
   [(:require
     [quil.core :as q]
     [shimmers.common.quil :as cq]
     [shimmers.math.geometry.group]
     [thi.ng.geom.core :as g]
     [thi.ng.geom.types])
    (:import
     (shimmers.math.geometry.group Group)
     (thi.ng.geom.types Circle2 Polygon2 Rect2 Triangle2))]
   :cljs
   [(:require
     [quil.core :as q :include-macros true]
     [shimmers.common.quil :as cq]
     [shimmers.math.geometry.group :refer [Group]]
     [thi.ng.geom.core :as g]
     [thi.ng.geom.types :refer [Circle2 Polygon2 Rect2 Triangle2]])]))

(defprotocol QuilDrawGeom
  (draw [s])
  (contour-draw [s]))

(defn draw-contour [vertices]
  (q/begin-contour)
  (doseq [v (reverse vertices)]
    (apply q/vertex v))
  (q/end-contour))

;; Requires (q/ellipse-mode :radius) to render as expected
(extend-type Circle2
  QuilDrawGeom
  (draw [s] (cq/circle s))
  (contour-draw [s] (draw-contour (g/vertices s))))

(extend-type Rect2
  QuilDrawGeom
  (draw [s] (cq/rectangle s))
  (contour-draw [s] (draw-contour (g/vertices s))))

(extend-type Triangle2
  QuilDrawGeom
  (draw [s]
    (apply cq/draw-triangle (:points s)))
  (contour-draw [s] (draw-contour (:points s))))

(extend-type Polygon2
  QuilDrawGeom
  (draw [s] (cq/draw-polygon s))
  (contour-draw [s] (draw-contour (g/vertices s))))

(extend-type Group
  QuilDrawGeom
  (draw [group]
    (doseq [s (:children group)]
      (draw s))))

(defrecord ContourPolygon [outer inners]
  QuilDrawGeom
  (draw [_]
    (q/begin-shape)
    (if (or (instance? Circle2 outer)
            (instance? Polygon2 outer))
      (doseq [v (g/vertices outer)]
        (apply q/vertex v))
      (draw outer))
    (doseq [inner inners]
      (contour-draw inner))
    (q/end-shape :close)))

(defn contour-polygon [outer inners]
  (ContourPolygon. outer inners))

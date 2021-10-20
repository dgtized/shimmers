(ns shimmers.common.quil-draws-geom
  (:require
   [shimmers.common.quil :as cq]
   #?(:clj [shimmers.math.geometry.group]
      :cljs [shimmers.math.geometry.group :refer [Group]])
   [thi.ng.geom.core :as g]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Circle2 Polygon2 Rect2 Triangle2]]))
  #?(:clj (:import [thi.ng.geom.types Circle2 Polygon2 Rect2 Triangle2]
                   [shimmers.math.geometry.group Group])))

(defprotocol QuilDrawGeom
  (draw [s]))

;; Requires (q/ellipse-mode :radius) to render as expected
(extend-type Circle2
  QuilDrawGeom
  (draw [{:keys [p r]}]
    (cq/circle p r)))

(extend-type Rect2
  QuilDrawGeom
  (draw [s]
    (cq/rectangle s)))

(extend-type Triangle2
  QuilDrawGeom
  (draw [s]
    (apply cq/draw-triangle (:points s))))

(extend-type Polygon2
  QuilDrawGeom
  (draw [s]
    (cq/draw-shape (g/vertices s))))

(extend-type Group
  QuilDrawGeom
  (draw [group]
    (doseq [s (:children group)]
      (draw s))))

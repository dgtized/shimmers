(ns shimmers.common.quil-draws-geom
  (:require [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as g]))

(defprotocol QuilDrawGeom
  (draw [s]))

;; Requires (q/ellipse-mode :radius) to render as expected
(extend-type thi.ng.geom.types.Circle2
  QuilDrawGeom
  (draw [{:keys [p r]}]
    (cq/circle p r)))

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
    (cq/draw-shape (g/vertices s))))

(extend-type shimmers.math.geometry.group.Group
  QuilDrawGeom
  (draw [group]
    (doseq [s (:children group)]
      (draw s))))

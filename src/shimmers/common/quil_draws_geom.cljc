(ns shimmers.common.quil-draws-geom
  (:require [shimmers.common.quil :as cq]
            shimmers.math.geometry.group
            [quil.core :as q]
            [thi.ng.geom.core :as geom]
            thi.ng.geom.types))

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
    (q/begin-shape)
    (doseq [v (geom/vertices s)]
      (apply q/vertex v))
    (q/end-shape :close)))

(extend-type shimmers.math.geometry.group.Group
  QuilDrawGeom
  (draw [group]
    (doseq [s (:children group)]
      (draw s))))

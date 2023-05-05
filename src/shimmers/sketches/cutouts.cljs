(ns shimmers.sketches.cutouts
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (mapcat (fn [[x0 x1]]
            (map (fn [[y0 y1]]
                   (let [sx (dr/random 0.005 (* 0.33 (- x1 x0)))
                         sy (dr/random 0.005 (* 0.2 (- y1 y0)))
                         r (dr/random 8.0)]
                     (vary-meta (rect/rect (rv (+ x0 sx) (+ y0 sy))
                                           (rv (- x1 sx) (- y1 sy)))
                                assoc :rx r :ry r)))
                 (partition 2 1 (dr/gaussian-range 0.08 0.02))))
          (partition 2 1 (dr/gaussian-range 0.05 0.01))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes)))

(sketch/definition cutouts
  {:created-at "2023-02-04"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :cutouts)))

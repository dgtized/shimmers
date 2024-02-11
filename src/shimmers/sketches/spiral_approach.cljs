(ns shimmers.sketches.spiral-approach
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.line :as gl]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn spiral [p n v s]
  (for [f (tm/norm-range n)
        :let [t (+ f s)]]
    (v/+polar p
              (* tm/PHI (Math/pow v (+ s (* 9 t))))
              (* 7 t eq/TAU))))

(defn shapes []
  (for [_s (tm/norm-range 10)]
    (gl/linestrip2 (spiral (rv 0.5 0.5)
                           (dr/random-int 1500 2500)
                           (+ tm/PHI (dr/gaussian 0.0 0.2))
                           (dr/gaussian 0.2 0.1)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.66}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :spiral-approach]
     [:div.readable-width
      "Experimenting with varying the exponential factor of a spiral."]]))

(sketch/definition spiral-approach
    {:created-at "2024-02-10"
     :tags #{}
     :type :svg}
  (ctrl/mount page))

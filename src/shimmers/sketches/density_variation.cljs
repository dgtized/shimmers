(ns shimmers.sketches.density-variation
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.line :as gl]
   [shimmers.math.equations :as eq]
   [clojure.math :as math]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.rect :as rect]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn fill [shape]
  (let [p0 (dr/random-tau)
        p1 (dr/random-tau)]
    (for [d (dr/density-range 0.002 0.01)]
      (let [n0 (Math/sin (+ p0 (* eq/TAU d) (Math/sin (+ p1 (* math/PI (- 1.0 d))))))
            n1 (Math/cos (+ p1 (* eq/TAU d) (Math/sin (+ p0 (* math/PI (- 1.0 d))))))]
        (gl/line2 (g/unmap-point shape (gv/vec2 d (+ 0.25 (* 0.15 n0))))
                  (g/unmap-point shape (gv/vec2 d (+ 0.75 (* 0.15 n1)))))))))

(defn shapes []
  (let [s0 (g/translate (g/scale-size (rect/rect 0 0 width height) 0.9) (rv 0 -0.05))
        s1 (g/translate (g/scale-size (rect/rect 0 0 width height) 0.9) (rv 0 0.05))]
    [(svg/group {} (fill s0))
     (svg/group {} (fill s1))]))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :density-variation]
     [:div.readable-width]]))

(sketch/definition density-variation
  {:created-at "2024-06-15"
   :tags #{}
   :type :svg}
  (ctrl/mount page))

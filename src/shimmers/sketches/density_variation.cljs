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

(defn densities []
  (let [samples (range 0 1 (dr/random 0.005 0.01))]
    ((dr/rand-nth [identity reverse])
     ((dr/weighted [[(fn [] (dr/density-range 0.002 0.01)) 1.0]
                    [(fn [] (let [v (Math/log 2.0)]
                             (map (fn [x] (/ (Math/log (inc x)) v)) samples))) 1.0]
                    [(fn [] samples)]])))))

(defn pairs []
  (let [p0 (dr/random-tau)
        p1 (dr/random-tau)]
    (for [d (densities)]
      (let [n0 (Math/sin (+ p0 (* eq/TAU d) (Math/sin (+ p1 (* math/PI (- 1.0 d))))))
            n1 (Math/cos (+ p1 (* eq/TAU d) (Math/sin (+ p0 (* math/PI (- 1.0 d))))))]
        [(gv/vec2 d (+ 0.25 (* 0.15 n0)))
         (gv/vec2 d (+ 0.75 (* 0.15 n1)))]))))

(defn fill [shape pairs]
  (for [[p q] pairs]
    (gl/line2 (g/unmap-point shape p)
              (g/unmap-point shape q))))

(defn shapes []
  (let [box (g/scale-size (rect/rect 0 0 width height) 0.85)]
    ((dr/weighted [[(fn [] (let [s0 (g/translate box (rv 0 -0.05))
                                s1 (g/translate box (rv 0 0.05))]
                            [(svg/group {} (fill s0 (pairs)))
                             (svg/group {} (fill s1 (pairs)))]))
                    1.0]
                   [(fn []
                      [(svg/group {} (fill box (pairs)))
                       (svg/group {} (fill box (map (fn [pair] (mapv reverse pair)) (pairs))))])
                    1.0]]))))

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

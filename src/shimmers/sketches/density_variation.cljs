(ns shimmers.sketches.density-variation
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn rescale [xs]
  (let [m (apply max xs)]
    (map (fn [x] (/ x m)) xs)))

(defn sin-density []
  (let [rate (dr/random-int 1 6)
        phase (dr/random-tau)
        scale (dr/random 0.0075 0.015)]
    (->> (range 0 1 0.005)
         (map (fn [x] (max 0.001 (* scale 0.5 (+ 1.0 (math/sin (+ phase (* eq/TAU rate x))))))))
         (reductions +)
         (take-while (fn [s] (<= s 1.0)))
         rescale)))

(defn invert [xs]
  (mapv (fn [x] (- 1.0 x)) (reverse xs)))

(defn densities []
  (let [samples (range 0 1 (dr/random 0.005 0.01))]
    ((dr/rand-nth [identity invert])
     ((dr/weighted [[(fn [] (dr/density-range 0.002 0.01)) 1.0]
                    [(fn [] (rescale (map (fn [x] (math/log (inc x))) samples))) 1.0]
                    [(fn [] (rescale (map (fn [x] (dec (math/pow (inc x) math/E))) samples))) 1.0]
                    [(fn [] samples) 1.0]
                    [(fn [] (dr/var-range (dr/random-int 100 250))) 1.0]
                    [sin-density 1.0]])))))

(defn pairs []
  (let [p0 (dr/random-tau)
        p1 (dr/random-tau)
        samples (densities)
        disp (if (dr/chance 0.5)
               (* (dr/random) (/ 0.8 (count samples)))
               0.0)]
    (for [d samples]
      (let [n0 (math/sin (+ p0 (* eq/TAU d) (* 1.5 (math/sin (+ p1 (* math/PI (- 1.0 d)))))))
            n1 (math/sin (+ p1 (* eq/TAU d) (* 1.5 (math/sin (+ p0 (* math/PI (- 1.0 d)))))))]
        [(gv/vec2 (dr/gaussian d disp) (+ 0.25 (* 0.15 n0)))
         (gv/vec2 (dr/gaussian d disp) (+ 0.75 (* 0.15 n1)))]))))

(defn fill [shape pairs]
  (println (count pairs))
  (for [[p q] pairs]
    (gl/line2 (g/unmap-point shape p)
              (g/unmap-point shape q))))

(defn shapes []
  (let [box (g/scale-size (csvg/screen width height) 0.85)]
    ((dr/weighted [[(fn [] (let [s0 (g/translate box (rv 0 -0.05))
                                s1 (g/translate box (rv 0 0.05))]
                            [(svg/group {} (fill s0 (pairs)))
                             (svg/group {} (fill s1 (pairs)))]))
                    1.0]
                   [(fn []
                      [(svg/group {} (fill box (pairs)))
                       (svg/group {} (fill box (map (fn [pair] (mapv reverse pair)) (pairs))))])
                    1.0]]))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes)))

(sketch/definition density-variation
  {:created-at "2024-06-15"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))

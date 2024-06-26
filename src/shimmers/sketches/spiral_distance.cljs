(ns shimmers.sketches.spiral-distance
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.math.core :as tm]))

;; concept here was to play with log or golden spirals as a means for assigning
;; color or depth to a region. Ie using distance to the closest point on the
;; spiral to displace into theta to assign some meaning.

;; https://en.wikipedia.org/wiki/List_of_spirals
;; https://en.wikipedia.org/wiki/Logarithmic_spiral
;; https://en.wikipedia.org/wiki/Golden_spiral
(defn log-spiral [alpha k theta]
  (v/polar (* alpha (math/exp (* k theta)))
           theta))

(defn noise-displace [factor r t p]
  (let [[x y] (tm/* p factor)
        n (q/noise x y t)]
    (v/+polar p r (* tm/TWO_PI n))))

(defn draw [_]
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/no-fill)
  (q/stroke-weight 0.25)
  (q/translate (cq/rel-vec 0.66 0.33))
  (let [t (/ (q/frame-count) 200)
        offset (- (/ 1 tm/PHI))
        dt (* 2 math/PI (tm/fract (/ t 5)))
        points (for [theta (range (* 2.5 math/PI) (* math/PI 17) 0.2)]
                 (log-spiral 0.1 0.175 (+ theta dt)))
        points (mapv (partial noise-displace (/ 1 400) 12 t) points)]
    (doseq [[[ia pa] [ib pb]] (partition 2 1 (map-indexed vector points))]
      (when-let [qa (nth points (- ia 28) nil)]
        (when-let [qb (nth points (- ib 22) nil)]
          (let [polygon (gp/polygon2 [pa qa qb pb])
                center (g/centroid polygon)
                mag (tm/mag center)
                c (+ offset (* 0.003 mag tm/PHI))]
            (q/fill (mod c 1.0) 0.6 (/ 1.1 tm/PHI) 1.0)
            (cq/draw-curve-shape (g/vertices polygon))))))))

(defn page []
  (sketch/component
   :size [800 600]
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition spiral-distance
  {:created-at "2021-10-18"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

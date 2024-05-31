(ns shimmers.sketches.circular-repetition
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.5)
  {:weights (repeatedly 3 #(dr/random -0.1 0.1))
   :osc (repeatedly 3 #(dr/random -0.2 0.2))})

(defn draw [{[a b c] :weights [o1 o2 o3] :osc}]
  (q/background 1.0)
  (q/with-translation [(cq/rel-vec 0.0 0.0)]
    (let [center (gv/vec2 0 0)
          r (cq/rel-h 0.4)
          t (/ (q/millis) 2000.0)]
      (dotimes [i 360]
        (q/push-matrix)
        (q/rotate-z (+ (* c (+ t i)) (* 2 (Math/sin (* o3 t)))))
        (q/rotate-y (+ (* b (+ t i)) (* 2 (math/sin (* o2 t)))))
        (q/rotate-x (+ (* a (+ t i)) (* 2 (math/sin (* o1 t)))))
        (cq/circle center r)
        (q/pop-matrix)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :renderer :p3d
     :setup setup
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition circular-repetition
  {:created-at "2024-05-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

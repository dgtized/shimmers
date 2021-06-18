(ns shimmers.sketches.flow-fields
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [shimmers.math.vector :as v]
            [thi.ng.math.core :as tm]
            [shimmers.common.quil :as cq]
            [shimmers.math.deterministic-random :as dr]))

(defn dir-at
  [[x y]]
  (* tm/TWO_PI (q/noise (/ x 100) (/ y 100))))

(defn draw-grid [size]
  (let [w (/ (q/width) size)
        h (/ (q/height) size)]
    (doseq [[p dir]
            (for [x (range (* -2 size) (* (+ 3 w) size) size)
                  y (range (* -2 size) (* (+ 3 h) size) size)]
              [(gv/vec2 x y) (dir-at [x y])])]
      (q/line p (v/add p (v/polar (* 0.5 size) dir))))))

(defn next-flow-point [p r]
  (tm/+ p (v/polar r (dir-at p))))

(defn flow-points [p r n]
  (reductions (fn [p] (next-flow-point p r)) p (range n)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:step-size 3
   :length 64})

(defn update-state [state]
  state)

(defn draw [{:keys [step-size length]}]
  (q/noise-seed (dr/random 1000000))
  (q/background 1.0)
  ;; (q/stroke-weight 0.1)
  ;; (q/stroke 0.0 0.0 0.0 1.0)
  ;; (draw-grid 10)
  (q/stroke-weight 0.2)
  (q/no-fill)
  (q/stroke 0.0 0.0 0.0 1.0)
  (time
   (dotimes [_ 3000]
     (q/begin-shape)
     (doseq [[x y] (flow-points (gv/vec2 (cq/rel-pos (dr/random) (dr/random)))
                                step-size length)]
       (q/curve-vertex x y))
     (q/end-shape))))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch flow-fields
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

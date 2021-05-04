(ns shimmers.sketches.falling-gradients
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/noise-seed (rand-int 100000))
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {})

(defn discrete-curve [slices phase scale offset]
  (for [x (range 0 1 (/ 1 slices))]
    [x (* scale (q/noise (* x phase) offset))]))

(defn draw [state]
  (q/background 1.0)
  (q/no-fill)
  (let [slices 200
        curve (discrete-curve slices 2 1.0 1000)
        depth-curve (map second (discrete-curve slices 5 0.4 50000))]
    (doseq [[[[x1 y1] [x2 _]] depth] (map vector (partition 2 1 curve) depth-curve)]
      (q/line (cq/rel-pos x1 y1) (cq/rel-pos x2 y1))
      (q/line (cq/rel-pos x1 y1) (cq/rel-pos x1 (+ y1 depth))))))

(defn ^:export run-sketch []
  ;; 20210504
  (q/defsketch falling-gradients
    :host "quil-host"
    :size [800 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

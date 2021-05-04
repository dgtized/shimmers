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

(defn discrete-curve [slices offset]
  (for [x (range 0 1 (/ 1 slices))]
    [x (q/noise (* x 2) offset)]))

(defn draw [state]
  (q/background 1.0)
  (q/no-fill)
  (doseq [[[x1 y1] [x2 _]] (partition 2 1 (discrete-curve 200 1000))]
    (q/line (cq/rel-pos x1 y1) (cq/rel-pos x2 y1))))

(defn ^:export run-sketch []
  ;; 20210504
  (q/defsketch falling-gradients
    :host "quil-host"
    :size [800 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

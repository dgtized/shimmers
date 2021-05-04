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

(defn update-state [state]
  state)

(defn draw [state]
  (let [slice-width 0.006]
    (q/background 1.0)
    (q/no-fill)
    (doseq [x (range 0 1 slice-width)]
      (let [y (+ (q/noise (* x 1.5) 1000))]
        (q/line (cq/rel-pos x y)
                (cq/rel-pos (+ x slice-width) y))))))

(defn ^:export run-sketch []
  ;; 20210504
  (q/defsketch falling-gradients
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

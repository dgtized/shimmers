(ns shimmers.sketches.colors
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (q/no-stroke)
  (let [dx 0.01
        dy 0.01
        l (/ (+ 1 (q/cos (/ (q/frame-count) 30))) 2)]
    (doseq [h (range 0 1 dx)]
      (doseq [s (range 0 1 dy)]
        (q/fill h s l 1.0)
        (q/rect (cq/rel-w h) (cq/rel-h s) (cq/rel-w dx) (cq/rel-h dy))))))

(defn ^:export run-sketch []
  (q/defsketch colors
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

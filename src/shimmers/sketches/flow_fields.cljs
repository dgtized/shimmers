(ns shimmers.sketches.flow-fields
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [shimmers.math.vector :as v]
            [thi.ng.math.core :as tm]))

(defn draw-grid [size]
  (let [w (/ (q/width) size)
        h (/ (q/height) size)]
    (doseq [[p dir]
            (for [x (range (* -2 size) (* (+ 3 w) size) size)
                  y (range (* -2 size) (* (+ 3 h) size) size)]
              [(gv/vec2 x y) (* tm/TWO_PI (q/noise (/ x 60) (/ y 60)))])]
      (q/line p (v/add p (v/polar (* 0.5 size) dir))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 1.0)
  (draw-grid 10))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch flow-fields
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

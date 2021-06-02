(ns shimmers.sketches.ring
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn setup []
  {:theta 0.0})

(defn update-state [state]
  (update state :theta + 0.08))

(defn draw [{:keys [theta]}]
  (q/background 255 4)
  (q/stroke 10 64)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [radial-noise (q/noise (q/cos (/ theta 2)) (q/sin (/ theta 2)))
        radius (+ 120 (* (- radial-noise 0.5) 10))
        x (* radius (q/cos theta))
        y (* radius (q/sin theta))]
    (q/stroke-weight (+ 0.8 radial-noise))
    (q/line [x y]
            (v/add (v/vec2 x y)
                   (v/polar (* radial-noise 32) (+ theta radial-noise))))))

(defn ^:export run-sketch []
  (q/defsketch ring
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

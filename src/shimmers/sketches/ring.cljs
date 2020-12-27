(ns shimmers.sketches.ring
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn setup []
  {:theta 0.0})

(defn update-state [state]
  (update state :theta + 0.08))

(defn draw [{:keys [theta]}]
  (q/background 255 2)
  (q/stroke 10 32)
  (q/stroke-weight 1)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [radius 120
        x (* radius (q/cos theta))
        y (* radius (q/sin theta))
        radial-noise (q/noise x y)]
    (q/line [x y]
            (v/add (v/vec2 x y)
                   (v/scale (v/unit2-from-angle theta)
                            (* radial-noise 32))))
    ))

(defn ^:export run-sketch []
  (q/defsketch ring
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

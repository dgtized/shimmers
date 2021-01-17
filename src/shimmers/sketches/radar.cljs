(ns shimmers.sketches.radar
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]))

(def green [0 230 0 200])

(defn setup []
  (let [hw (/ (q/width) 2)
        hh (/ (q/height) 2)
        radius (* 0.45 (q/height))]
    (apply q/stroke green)
    (apply q/fill green)
    (q/ellipse-mode :radius)
    (q/ellipse hw hh radius radius)
    {:theta 0.0
     :center (v/vec2 hw hh)
     :radius radius
     :point (v/vec2 (- radius) 20)}))

(defn update-state [state]
  (-> state (update :theta + 0.01)
      (update :point v/add (v/vec2 0.1 -0.0001))))

(defn draw [{:keys [theta center radius point]}]
  (q/background 0 3)
  (apply q/stroke green)
  (apply q/translate center)
  (q/stroke-weight 2)
  (q/point 0 0)
  (q/point 10 10)
  (q/stroke-weight 0.8)
  (q/line 0 0 (* radius (q/cos theta)) (* radius (q/sin theta))))

(defn ^:export run-sketch []
  (q/defsketch radar
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

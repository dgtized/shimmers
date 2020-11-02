(ns shimmers.cube
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

(defn rectangle [[x y] width height]
  [(v/vec2 x y) (v/vec2 (+ x width) y)
   (v/vec2 (+ x width) (+ y height))
   (v/vec2 x (+ y height))])

(defn setup []
  {:vertices (rectangle [0 0] 50 50)})

(defn update-state [state]
  state)

(defn draw [{:keys [vertices]}]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (q/point 150 150)
  (q/with-translation [200 200]
    (q/with-rotation [(mod (/ (q/frame-count) 100) 360)]
      (q/begin-shape)
      (doseq [vertex vertices]
        (apply q/vertex vertex))
      (apply q/vertex (first vertices))
      (q/end-shape)))
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch cube
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))



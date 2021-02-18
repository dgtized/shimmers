(ns shimmers.sketches.ascendance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn setup []
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 255 16)
  (q/color 0 16)
  (let [h (q/height)
        t (/ (q/frame-count) 10)]
    (println [h t (/ h (mod t h))])
    (q/point (* 150 (q/cos t))
             (q/map-range (mod (* 2 t) h) 0 h 200 -200)
             (* 150 (q/sin t)))))

(defn ^:export run-sketch []
  (q/defsketch template
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

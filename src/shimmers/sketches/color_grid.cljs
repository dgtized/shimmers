(ns shimmers.sketches.color-grid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn sample-color [x y]
  [(/ (+ x y) 12) (+ 0.2 (/ x 12)) (+ 0.2 (/ y 8)) 1])

(defn make-grid [cols rows]
  (apply merge
         (for [r (range rows)
               c (range cols)]
           {[c r] (sample-color (inc c) (inc r))})))

(defn setup []
  {:grid (make-grid 6 4)})

(defn update-state [state]
  state)

(defn draw [{:keys [grid]}]
  (q/color-mode :hsl 1 1 1 1)
  (q/rect-mode :corner)
  (let [w (/ (q/width) 6)
        h (/ (q/height) 4)]
    (doseq [[[c r] color] grid]
      (apply q/fill color)
      (q/rect (* c w) (* r h) w h))))

(defn ^:export run-sketch []
  (q/defsketch color-grid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

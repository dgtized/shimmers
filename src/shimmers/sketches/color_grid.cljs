(ns shimmers.sketches.color-grid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn sample-color [x y]
  [(/ (+ x y) 20) (+ 0.2 (/ x 20)) (+ 0.2 (/ y 12)) 1])

(defn make-grid [cols rows]
  {:dims [cols rows]
   :grid
   (apply merge
          (for [r (range rows)
                c (range cols)]
            {[c r] (sample-color (inc c) (inc r))}))})

(defn setup []
  (make-grid 12 8))

(defn update-state [state]
  state)

(defn draw [{:keys [grid dims]}]
  (q/color-mode :hsl 1 1 1 1)
  (q/rect-mode :corner)
  (let [[cols rows] dims
        w (/ (q/width) cols)
        h (/ (q/height) rows)]
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

(ns shimmers.sketches.iterative-displacement
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]))

(defn vline [x]
  [(cq/rel-pos x 0.1) (cq/rel-pos x 0.9)])

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines
   (concat [(vline 0)]
           (map vline (range 0.1 0.9 0.1))
           [(vline 1.0)])})

(defn update-state [state]
  state)

(defn draw [{:keys [lines]}]
  (q/no-fill)
  (doseq [points (butlast (rest lines))]
    (q/begin-shape)
    (doseq [[x y] points]
      (q/vertex x y))
    (q/end-shape)))

(sketch/defquil iterative-displacement
  :created-at "2021-08-22"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

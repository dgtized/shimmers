(ns shimmers.sketches.inconsequential-drift
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]
            [thi.ng.math.core :as tm]))

(defn square-grid [size]
  (for [x (range size)
        y (range size)]
    {:pos (gv/vec2 x y)
     :width (tm/random 0.5 0.9)
     :height (tm/random 0.5 0.9)}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 30)
  (let [size 25]
    {:size size
     :grid (square-grid size)}))

(defn update-state [state]
  state)

(defn draw [{:keys [size grid]}]
  (q/background 1.0 0.5)
  (q/ellipse-mode :radius)
  (let [scale (/ (q/width) size)
        base (gv/vec2 (/ scale 2) (/ scale 2))]
    (doseq [{:keys [pos width height]} grid
            :let [[x y] (tm/+ base (tm/* pos scale))]]
      (q/ellipse x y (* width scale 0.5) (* height scale 0.5)))))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch inconsequential-drift
    :host "quil-host"
    :size [800 800]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

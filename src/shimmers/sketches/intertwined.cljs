(ns shimmers.sketches.intertwined
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [b (cq/screen-rect 0.95)
        zones (g/subdivide b {:rows 5 :cols 5})
        k (* 0.2 (count zones))
        zones (cons (first zones) (drop k (shuffle (rest zones))))]
    {:path (map g/centroid zones)}))

(defn update-state [state]
  state)

(defn draw [{:keys [path]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (cq/draw-curve-path path))

(sketch/defquil intertwined
  :created-at "2021-10-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

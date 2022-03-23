(ns shimmers.sketches.voronoi-after-effect
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.95)]
    {:bounds bounds
     :points (rp/poisson-disc-sampling bounds 128)}))

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/background 1.0 0.01)
  (q/stroke 0.0 0.33)
  (let [polygons (delvor/voronoi-cells points (cq/screen-rect))]
    (doseq [shape polygons]
      (cq/draw-shape (g/vertices shape)))))

(sketch/defquil voronoi-after-effect
  :created-at "2022-03-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.delaunay-voronoi
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.utils.delaunay :as delaunay]))

(defn generate-points [n dist]
  (repeatedly n #(gv/vec2 (dist) (dist))))

(defn setup []
  {:points (generate-points 30 #(q/random 0.15 0.85))})

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/background 255)
  (q/ellipse-mode :radius)
  (doseq [p points
          :let [[x y] (cq/rel-pos p)]]
    (q/ellipse x y 0.2 0.2))
  (q/stroke-weight 0.5)
  (doseq [triangle (delaunay/triangulate points)]
    (apply q/triangle (mapcat cq/rel-pos triangle))))

(defn ^:export run-sketch []
  (q/defsketch delaunay-voronoi
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

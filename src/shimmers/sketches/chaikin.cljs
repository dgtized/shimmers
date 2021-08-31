(ns shimmers.sketches.chaikin
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.chaikin :as chaikin]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.utils.subdiv :as gsd]
            [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw-at [vertices pos]
  (let [translated (map (fn [v] (geom/translate v pos)) vertices)]
    (q/begin-shape)
    (doseq [v translated]
      (apply q/vertex v))
    (q/end-shape)
    (cq/circle (first translated) 3.0)))

(defn draw [state]
  (q/background 1.0)
  (let [shapes [(rect/rect (cq/rel-pos 0.1 0.1) (cq/rel-pos 0.2 0.2))]]
    (doseq [shape shapes
            :let [vertices (geom/vertices shape)]]
      (draw-at vertices (gv/vec2))
      (draw-at (chaikin/chaikin-closed vertices 0.25) (cq/rel-vec 0.2 0.0))
      (draw-at (gsd/subdivide-closed (:chaikin gsd/schemes) vertices) (cq/rel-vec 0.4 0.0))
      (draw-at (chaikin/chaikin-open vertices 0.25) (cq/rel-vec 0.2 0.2)))))

(sketch/defquil chaikin
  :created-at "2021-08-31"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

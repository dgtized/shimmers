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

(def gsd-chaikin (partial gsd/subdivide-closed (:chaikin gsd/schemes)))

(defn draw [state]
  (q/background 1.0)
  (let [shapes [(rect/rect (cq/rel-pos 0.05 0.05) (cq/rel-pos 0.15 0.15))]]
    (doseq [shape shapes
            :let [vertices (geom/vertices shape)]]
      (draw-at vertices (gv/vec2))
      (draw-at (chaikin/chaikin-closed vertices 0.25) (cq/rel-vec 0.2 0.0))
      (draw-at (chaikin/chaikin 0.25 true 2 vertices) (cq/rel-vec 0.4 0.0))
      (draw-at (chaikin/chaikin 0.25 true 3 vertices) (cq/rel-vec 0.6 0.0))
      (draw-at (chaikin/chaikin 0.25 true 4 vertices) (cq/rel-vec 0.8 0.0))
      (draw-at (chaikin/chaikin-open vertices 0.25) (cq/rel-vec 0.2 0.2))
      (draw-at (chaikin/chaikin 0.25 false 2 vertices) (cq/rel-vec 0.4 0.2))
      (draw-at (chaikin/chaikin 0.25 false 3 vertices) (cq/rel-vec 0.6 0.2))
      (draw-at (chaikin/chaikin 0.25 false 4 vertices) (cq/rel-vec 0.8 0.2))
      (draw-at (gsd-chaikin vertices) (cq/rel-vec 0.2 0.4))
      (draw-at (gsd-chaikin (gsd-chaikin vertices)) (cq/rel-vec 0.4 0.4))
      (draw-at (gsd-chaikin (gsd-chaikin (gsd-chaikin vertices))) (cq/rel-vec 0.6 0.4))
      (draw-at (gsd-chaikin (gsd-chaikin (gsd-chaikin (gsd-chaikin vertices)))) (cq/rel-vec 0.8 0.4))
      )))

(sketch/defquil chaikin
  :created-at "2021-08-31"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

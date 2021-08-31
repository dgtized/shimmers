(ns shimmers.sketches.chaikin
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.algorithm.chaikin :as chaikin]
            [thi.ng.geom.rect :as rect]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (let [shapes [(rect/rect (cq/rel-pos 0.1 0.1) (cq/rel-pos 0.2 0.2))]]
    (doseq [shape shapes]
      (cq/draw-shape (geom/vertices shape))
      (cq/draw-shape (chaikin/chaikin (geom/vertices (geom/translate shape (cq/rel-vec 0.2 0))) 0.25 true))
      (cq/draw-shape (chaikin/chaikin (geom/vertices (geom/translate shape (cq/rel-vec 0.2 0.2))) 0.25 false)))))

(sketch/defquil chaikin
  :created-at "2021-08-31"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.stem-and-leaf
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (gv/vec2 0 0) (cq/rel-h 0.1))]})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [{:keys [p r]} circles]
      (cq/circle p r))))

(sketch/defquil stem-and-leaf
  :created-at "2021-07-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

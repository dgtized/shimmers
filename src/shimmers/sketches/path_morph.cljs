(ns shimmers.sketches.path-morph
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.utils :as gu]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [shimmers.common.quil :as cq]))

;; TODO: define a path record from a set of points
(defn zig-zag []
  (gp/polygon2 [[0.2 0.2] [0.8 0.2]
                [0.2 0.8] [0.8 0.8]]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:path (zig-zag)
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.001))

(defn draw [{:keys [path t]}]
  (q/background 1.0 0.1)
  (let [bounds (rect/rect 0 0 (q/width) (q/height))
        p (geom/unmap-point bounds (gu/point-at (mod t 1.0) (:points path)))]
    (cq/circle p 20)))

(sketch/defquil path-morph
  :created-at "2021-10-06"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

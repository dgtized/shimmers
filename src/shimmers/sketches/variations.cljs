(ns shimmers.sketches.variations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn hashmark []
  [(gl/line2 0.4 0.1 0.4 0.9)
   (gl/line2 0.6 0.1 0.6 0.9)
   (gl/line2 0.1 0.4 0.9 0.4)
   (gl/line2 0.1 0.6 0.9 0.6)])

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw-mark [pos scale]
  (doseq [{[p q] :points} (hashmark)]
    (q/line (tm/+ pos (tm/* p scale))
            (tm/+ pos (tm/* q scale)))))

(defn draw [state]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (draw-mark (gv/vec2) 400.0))

(sketch/defquil variations
  :created-at "2021-08-25"
  :size [400 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.deformed-spirals
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.math.core :as tm]))

(defn spiral [center dr dtheta steps]
  (for [theta (range 0 (* steps dtheta) dtheta)]
    (geom/translate (v/polar (* dr (/ theta tm/TWO_PI)) theta)
                    center)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 1.0)
  (q/no-fill)
  (q/begin-shape)
  (doseq [v (spiral (cq/rel-vec 0.5 0.5) 8.0 0.9 200)]
    (apply q/curve-vertex v))
  (q/end-shape))

(sketch/defquil deformed-spirals
  :created-at "2021-10-10"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

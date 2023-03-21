(ns shimmers.sketches.liminal-space
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]))

(defn inverted [x]
  (- 1.0 x))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [x ((dr/rand-nth [inverted identity])
           (dr/weighted {(/ 2 7) 1
                         (/ 2 5) 1
                         (/ 1 4) 1
                         (/ 1 3) 1}))
        boundary (gl/line2 (cq/rel-vec x -1.0) (cq/rel-vec x 2.0))
        angle (dr/gaussian 0.0 0.1)]
    {:boundary (geometry/rotate-around-centroid boundary angle)}))

(defn update-state [state]
  state)

(defn draw [{:keys [boundary]}]
  (q/background 1.0)
  (qdg/draw boundary))

(sketch/defquil liminal-space
  :created-at "2023-03-21"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

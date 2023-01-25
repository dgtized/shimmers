(ns shimmers.sketches.hyphae
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:points
   (repeatedly 128 (fn [] (gc/circle (v/+polar (cq/rel-vec 0.5 0.5)
                                              (cq/rel-h (Math/sqrt (dr/random 0.125 0.2)))
                                              (dr/random eq/TAU))
                                    (cq/rel-h (dr/random 0.01 0.05)))))})

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [c points]
    (cq/circle c)))

(sketch/defquil hyphae
  :created-at "2023-01-24"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

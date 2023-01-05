(ns shimmers.sketches.intersecting-circle-regions
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.types :refer [Circle2]]
   [thi.ng.geom.core :as g]))

(defn regions [{pa :p ra :r :as a} {pb :p rb :r :as b}]
  (if (collide/overlaps? a b)
    [a b]
    [a b]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (cq/rel-vec 0.35 0.5) (cq/rel-h 0.4))
             (gc/circle (cq/rel-vec 0.65 0.5) (cq/rel-h 0.4))]})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke 0.0)
  (doseq [circle circles]
    (cq/circle circle))

  (q/stroke 0.5)
  (doseq [region (regions (first circles) (second circles))]
    (cond (instance? Circle2 region)
          (cq/circle (g/scale-size region 0.9)))))

(sketch/defquil intersecting-circle-regions
  :created-at "2023-01-05"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

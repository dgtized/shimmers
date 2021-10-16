(ns shimmers.sketches.motif-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.common.quil-draws-geom :as qdg]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.utils :as gu]
            [shimmers.common.quil :as cq]))

(defn square []
  [(rect/rect 0 0 1 1)])

(defn tile-grid
  ([bounds shape-groups] (tile-grid bounds shape-groups {:scale 0.9}))
  ([bounds shape-groups {:keys [scale]}]
   (let [n (count shape-groups)
         cols (tm/ceil (Math/sqrt n))
         rows (tm/ceil (/ n cols))
         tiles (take n (geom/subdivide bounds {:cols cols :rows rows}))]
     (mapcat (fn [group tile]
               (-> tile
                   (geom/scale-size scale)
                   (gu/fit-all-into-bounds group)))
             shape-groups tiles))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shape-groups (repeatedly 10 square)})

(defn update-state [state]
  state)

(defn draw [{:keys [shape-groups]}]
  (q/background 1.0)
  (doseq [s (tile-grid (rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.9 0.9))
                       shape-groups)]
    (qdg/draw s)))

(sketch/defquil motif-shapes
  :created-at "2021-10-16"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

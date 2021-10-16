(ns shimmers.sketches.motif-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.math.core :as tm]))

(defn circle []
  [(gc/circle [0.5 0.5] 0.5)])

(defn square []
  [(rect/rect 0 0 1 1)])

(defn triangle []
  [(gt/triangle2 [0 0] [0 1] [1 0])])

(defn cardinal-direction []
  (* 2 Math/PI (rand-nth (tm/norm-range 4))))

(defn diagonal-direction []
  (+ (/ Math/PI 4) (cardinal-direction)))

(defn group-translate [group r theta]
  (mapcat (fn [s]
            [s (geom/translate s (v/polar r theta))])
          group))

(defn group-rotation [group theta]
  (let [group-centroid (tm/div (reduce tm/+ (map geom/centroid group)) (count group))]
    (for [shape group]
      (-> shape
          (geom/center group-centroid)
          (geom/rotate theta)
          (geom/translate (tm/- group-centroid (geom/centroid shape)))))))

(def legal-shapes [circle square triangle])

(defn rotated-shape []
  (group-rotation ((rand-nth legal-shapes))
                  (cardinal-direction)))

(defn overlap-shape []
  (group-translate ((rand-nth legal-shapes))
                   (rand-nth [0.1 0.2 0.5])
                   (diagonal-direction)))

(defn random-shape []
  ((rand-nth (into legal-shapes [rotated-shape overlap-shape]))))

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
  (q/ellipse-mode :radius)
  {:shape-groups (repeatedly 30 random-shape)})

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

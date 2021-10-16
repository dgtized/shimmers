(ns shimmers.sketches.motif-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.probability :as p]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn circle []
  [(gc/circle [0.5 0.5] 0.5)])

(defn square []
  [(rect/rect 0 0 1 1)])

(defn rectangle []
  (rand-nth [[(rect/rect 0 0 0.66 0.5)]
             [(rect/rect 0 0 0.5 0.66)]]))

(defn triangle []
  [(gt/triangle2 [0 0] [0 1] [1 0])])

(defn cardinal-direction []
  (* 2 Math/PI (rand-nth (tm/norm-range 4))))

(defn diagonal-direction []
  (+ (/ Math/PI 4) (cardinal-direction)))

(defn group-translate [group offset]
  (map (fn [s] (geom/translate s offset))
       group))

(defn group-rotation [group theta]
  (let [group-centroid (tm/div (reduce tm/+ (map geom/centroid group)) (count group))]
    (for [shape group]
      (-> shape
          (geom/center group-centroid)
          (geom/rotate theta)
          (geom/translate (tm/- group-centroid (geom/centroid shape)))))))

(defn group-duplicate [group direction]
  (let [bounds (gu/coll-bounds group)
        offset (case direction
                 :x (gv/vec2 (geom/width bounds) 0)
                 :y (gv/vec2 0 (geom/height bounds)))]
    (concat group (group-translate group (tm/* offset 1.1)))))

(def legal-shapes [circle square rectangle triangle])

(defn rotated-shape []
  (group-rotation ((rand-nth legal-shapes))
                  (cardinal-direction)))

(declare random-shape)

(defn overlap-shape []
  (let [s (random-shape)
        dir (v/polar (rand-nth [0.1 0.2 0.5 1.0])
                     (diagonal-direction))]
    (concat s (group-translate s dir))))

(defn duplicate-shape []
  (group-duplicate (random-shape) (rand-nth [:x :y])))

(defn random-shape []
  ((p/weighted {rotated-shape 1.0
                overlap-shape 0.4
                duplicate-shape 0.2})))

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
  {:shape-groups (repeatedly 64 random-shape)})

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

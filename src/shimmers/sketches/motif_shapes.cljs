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
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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

(defn circle []
  [(gc/circle [0.5 0.5] 0.5)])

(defn n-gon [n]
  [(gp/polygon2 (geom/vertices (first (circle)) n))])

(defn square []
  [(rect/rect 0 0 1 1)])

(defn rectangle []
  (rand-nth [[(rect/rect 0 0 0.66 0.5)]
             [(rect/rect 0 0 0.5 0.66)]]))

(defn right-triangle []
  [(gt/triangle2 [0 0] [0 1] [1 0])])

(defn triangle []
  [(gt/triangle2 [0 0] [0 1] [0.5 0.5])])

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

(defn group-copies [group direction copies]
  (let [bounds (gu/coll-bounds group)
        offset (case direction
                 :x (gv/vec2 (geom/width bounds) 0)
                 :y (gv/vec2 0 (geom/height bounds)))]
    (mapcat (fn [v] (group-translate group (tm/* (tm/* offset 1.1) v)))
            (range copies))))

(defn group-mirror [group direction]
  (let [bounds (gu/coll-bounds group)
        offset (tm/abs (rect/bottom-left bounds))
        ;; ensure entire group is inside of upper-right quadrant before mirroring
        g (group-translate group (tm/* offset 1.1))
        dir (case direction
              :x (mat/matrix32 -1.0 0 0 0 1 0)
              :y (mat/matrix32 1 0 0 0 -1.0 0))]
    (concat g
            (mapv (fn [s] (geom/transform s dir)) g))))

(def shape-distribution
  {circle 5
   (partial n-gon 5) 2
   (partial n-gon 6) 3
   (partial n-gon 8) 2
   (partial n-gon 12) 1
   square 6
   rectangle 3
   triangle 4
   right-triangle 4})

(defn rotated-shape []
  (group-rotation ((p/weighted shape-distribution)) (cardinal-direction)))

(defn shape-sequence []
  (let [dir (rand-nth [(gv/vec2 1.1 0) (gv/vec2 0 1.1)])]
    (loop [shapes [] n (p/weighted {2 8 3 4 4 2})
           base (gv/vec2)]
      (if (zero? n)
        shapes
        (let [s (geom/center (first (rotated-shape)))
              bounds (geom/bounds s)
              size (gv/vec2 (geom/width bounds) (geom/height bounds))
              space (tm/* size dir)]
          (recur (conj shapes (geom/translate s base))
                 (dec n)
                 (tm/+ base space)))))))

(declare random-shape)

(defn overlap-shape []
  (let [s (random-shape)
        dir (v/polar (rand-nth [0.1 0.2 0.5 1.0])
                     (diagonal-direction))]
    (concat s (group-translate s dir))))

(defn duplicate-shape []
  (group-copies (random-shape)
                (rand-nth [:x :y])
                (p/weighted {2 8
                             3 4
                             4 2
                             5 1})))

(defn mirror-shape []
  (group-mirror (random-shape) (rand-nth [:x :y])))

(defn grid-shape []
  (let [n (p/weighted {3 2
                       4 8
                       8 2
                       9 4
                       14 1
                       15 1
                       16 2})]
    (group-rotation (tile-grid (rect/rect [0.0 0.0] [1.0 1.0])
                               (repeatedly n rotated-shape))
                    (cardinal-direction))))

(defn random-shape []
  ((p/weighted {rotated-shape 1.0
                grid-shape 0.1
                shape-sequence 0.2
                overlap-shape 0.1
                duplicate-shape 0.2
                mirror-shape 0.2})))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:shapes (->> (repeatedly 64 random-shape)
                (tile-grid (rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.9 0.9)))
                (random-sample 0.95))})

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (doseq [s shapes]
    (qdg/draw s)))

(sketch/defquil motif-shapes
  :created-at "2021-10-16"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

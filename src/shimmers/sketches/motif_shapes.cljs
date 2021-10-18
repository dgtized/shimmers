(ns shimmers.sketches.motif-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.geometry.group :as gg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn fit-grid [n]
  (let [cols (tm/ceil (Math/sqrt n))
        rows (tm/ceil (/ n cols))]
    [cols rows (- (* cols rows) n)]))

;; TODO: support odd grids like 4:3:4 and the like
(defn tile-grid
  ([bounds shape-groups] (tile-grid bounds shape-groups {:scale 0.9}))
  ([bounds shape-groups {:keys [scale]}]
   (let [n (count shape-groups)
         [rows cols _] (fit-grid n)
         tiles (take n (g/subdivide bounds {:cols cols :rows rows}))]
     (gg/group (mapcat (fn [group tile]
                         (-> tile
                             (g/scale-size scale)
                             (gu/fit-all-into-bounds (:children group))))
                       shape-groups tiles)))))

(defn circle []
  (gc/circle [0.5 0.5] 0.5))

(defn n-gon [n]
  (gp/polygon2 (g/vertices (circle) n)))

(defn square []
  (rect/rect 0 0 1 1))

(defn rectangle []
  (->> [(rect/rect 0 0 0.66 0.5)
        (rect/rect 0 0 0.5 0.66)]
       dr/rand-nth))

(defn right-triangle []
  (gt/triangle2 [0 0] [0 1] [1 0]))

(defn triangle []
  (gt/triangle2 [0 0] [0 1] [0.5 0.5]))

(defn cardinal-direction []
  (* 2 Math/PI (dr/rand-nth (tm/norm-range 4))))

(defn diagonal-direction []
  (+ (/ Math/PI 4) (cardinal-direction)))

(defn group-rotation [{:keys [children] :as group} theta]
  (let [group-centroid (g/centroid group)]
    (gg/group (for [shape children]
                (-> shape
                    (g/center group-centroid)
                    (g/rotate theta)
                    (g/translate (tm/- group-centroid (g/centroid shape))))))))

(defn group-copies [group direction copies]
  (let [offset (case direction
                 :x (gv/vec2 (g/width group) 0)
                 :y (gv/vec2 0 (g/height group)))]
    (gg/group (mapcat (fn [v] (:children (g/translate group (tm/* (tm/* offset 1.1) v))))
                      (range copies)))))

(defn group-mirror [group direction]
  (let [bounds (g/bounds group)
        offset (tm/abs (rect/bottom-left bounds))
        ;; ensure entire group is inside of upper-right quadrant before mirroring
        g (g/translate group (tm/* offset 1.1))
        dir (case direction
              :x (mat/matrix32 -1.0 0 0 0 1 0)
              :y (mat/matrix32 1 0 0 0 -1.0 0))]
    (gg/group (concat (:children g)
                      (mapv (fn [s] (g/transform s dir)) (:children g))))))

(def shape-limit 48)
(def shape-distribution
  {circle 8
   (partial n-gon 5) 2
   (partial n-gon 6) 3
   (partial n-gon 8) 2
   (partial n-gon 12) 1
   square 12
   rectangle 4
   triangle 6
   right-triangle 6})

(defn rotated-shape []
  (let [shape ((dr/weighted shape-distribution))]
    (group-rotation (gg/group shape) (cardinal-direction))))

(defn shape-sequence []
  (let [dir (dr/rand-nth [(gv/vec2 1.1 0) (gv/vec2 0 1.1)])]
    (loop [shapes [] n (dr/weighted {2 8 3 4 4 2})
           base (gv/vec2)]
      (if (zero? n)
        (gg/group shapes)
        (let [shape (first (:children (rotated-shape)))
              s (g/center shape)
              space (tm/* (:size (g/bounds s)) dir)]
          (recur (conj shapes (g/translate s base))
                 (dec n)
                 (tm/+ base space)))))))

(declare random-shape)

(defn overlap-shape []
  (let [group (random-shape)
        dir (v/polar (dr/rand-nth [0.1 0.2 0.5 1.0])
                     (diagonal-direction))]
    (if (> (gg/count-children group) shape-limit)
      group
      (gg/group (concat (:children group) (:children (g/translate group dir)))))))

(defn duplicate-shape []
  (let [group (random-shape)]
    (if (> (gg/count-children group) shape-limit)
      group
      (group-copies group
                    (dr/rand-nth [:x :y])
                    (dr/weighted {2 8
                                  3 4
                                  4 2
                                  5 1})))))

(defn mirror-shape []
  (let [group (random-shape)]
    (if (> (gg/count-children group) shape-limit)
      group
      (group-mirror group (dr/rand-nth [:x :y])))))

(defn grid-shape []
  (let [n (dr/weighted {3 2
                        4 8
                        8 2
                        9 4
                        12 1
                        14 1
                        15 1
                        16 2})]
    (group-rotation (tile-grid (rect/rect [0.0 0.0] [1.0 1.0])
                               (repeatedly n rotated-shape))
                    (cardinal-direction))))

(defn random-shape []
  ((dr/weighted {rotated-shape 5
                 grid-shape 2
                 shape-sequence 2
                 overlap-shape 1
                 duplicate-shape 1
                 mirror-shape 1})))

;; (keep (fn [x] (let [f (fit-grid x)] (when (zero? (nth f 2)) [x f]))) (range 10 400))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (let [screen-sizes {64 1
                      81 2
                      110 3
                      144 4
                      156 2
                      256 1}]
    {:shapes (->> (repeatedly (dr/weighted screen-sizes) random-shape)
                  (tile-grid (rect/rect (cq/rel-vec 0.05 0.05) (cq/rel-vec 0.95 0.95))))}))

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/stroke-weight 0.66)
  (qdg/draw shapes))

;; Convert to SVG?

(defn page []
  [:p.center (view-sketch/generate :motif-shapes)])

(sketch/defquil motif-shapes
  :created-at "2021-10-16"
  :tags #{:static :deterministic}
  :on-mount #(ctrl/mount page)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

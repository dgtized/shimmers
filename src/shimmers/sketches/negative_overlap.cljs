(ns shimmers.sketches.negative-overlap
  (:require
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-rect []
  (let [w (dr/random)
        h (dr/random)
        x (dr/random)
        y (dr/random)]
    (assoc (g/as-polygon (rect/rect (rv x y) (rv w h)))
           :open true)))

(defn good-shape? [poly]
  (let [{[w h] :size} (g/bounds poly)]
    (and (> (g/area poly) 10)
         (> w 20)
         (> h 20))))

(def base-shape
  (assoc (g/as-polygon (rect/rect 0 0 width height)) :open true))

(defn assign-open [shapes open]
  (map #(assoc (g/as-polygon %) :open open) shapes))

(defn xor-fill [{a :open} {b :open}]
  (= (bit-xor a b) 1))

(defn same-shape? [{a :points} {b :points}]
  (every? true? (map tm/delta= a b)))

;; for now this is removing the clip each time
(defn poly-exclusion [a b]
  (let [clip (g/clip-with a b)]
    (if (empty? (:points clip)) ;; no intersection between pair
      [a]
      (->> (cond (same-shape? clip a) ;; a is contained by b
                 (assign-open (square/surrounding-panes (g/bounds b) (g/bounds a) :row)
                              (:open b))
                 (same-shape? clip b) ;; b is contained by a
                 (assign-open (square/surrounding-panes (g/bounds a) (g/bounds b) :column)
                              (:open a))
                 :else ;; partial overlap
                 (concat (assign-open (square/surrounding-panes (g/bounds a) (g/bounds clip) :column)
                                      (:open a))
                         (assign-open (square/surrounding-panes (g/bounds b) (g/bounds clip) :row)
                                      (:open b))))
           (filter (comp square/has-area? g/bounds))
           (into (assign-open [clip] (xor-fill a b)))))))

(comment
  (assert (= (g/clip-with (g/as-polygon (rect/rect 0 0 10 10))
                          (g/as-polygon (rect/rect 12 12 10 10)))
             (gp/polygon2 [])))

  (poly-exclusion base-shape (random-rect))
  (poly-exclusion (random-rect) (random-rect)))

;; still finding gaps, need to include clip + deal with zero width/height?
(defn split-shapes [base all-shapes]
  (reduce (fn [shapes s]
            (let [shapes' (mapcat #(poly-exclusion % s) shapes)]
              (if (= (count shapes') (count shapes))
                (conj shapes' s)
                shapes')))
          [base]
          all-shapes))

(defn shapes []
  (->> random-rect
       repeatedly
       (filter good-shape?)
       (take 2)
       (split-shapes base-shape)))

(defn fill-shape [{:keys [open] :as shape}]
  (vary-meta shape assoc :fill
             (if open "lightblue" "grey")))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (apply list (map fill-shape (shapes)))))

(sketch/definition negative-overlap
  {:created-at "2022-01-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :negative-overlap)
              "sketch-host"))

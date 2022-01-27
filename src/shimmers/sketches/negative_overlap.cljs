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
   [thi.ng.geom.svg.core :as svg]
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

(defn big-enough? [poly]
  (let [{[w h] :size} (g/bounds poly)]
    (and (> w 20) (> h 20))))

(def base-shape
  (assoc (g/as-polygon (rect/rect 0 0 width height)) :open false))

(defn assign-open [shapes open]
  (map #(assoc (g/as-polygon %) :open open) shapes))

(defn xor-fill [{a :open} {b :open}]
  (= (bit-xor a b) 1))

(defn same-shape? [{a :points} {b :points}]
  (every? true? (map tm/delta= a b)))

;; for now this is removing the clip each time
(defn poly-exclusion [a b]
  (let [clip (g/clip-with a b)
        {point-a :p :as rect-a} (g/bounds a)
        {point-b :p :as rect-b} (g/bounds b)
        {point-clip :p :as rect-clip} (g/bounds clip)
        t-clip (g/translate rect-clip (tm/- point-clip))]
    (if (empty? (:points clip)) ;; no intersection between pair
      [a]
      (->> (cond (same-shape? clip b) ;; b is contained by a
                 (map #(g/translate % point-a)
                      (concat (assign-open (square/surrounding-panes rect-a rect-b :row)
                                           (:open a))
                              (assign-open [clip] (xor-fill a b))))
                 (same-shape? clip a) ;; a is contained by b
                 (map #(g/translate % point-b)
                      (concat (assign-open (square/surrounding-panes rect-b rect-a :row)
                                           (:open b))
                              (assign-open [clip] (xor-fill a b))))
                 :else ;; partial overlap
                 (let [panes (square/surrounding-panes (g/translate rect-a (tm/- point-a)) t-clip
                                                       :row)]
                   (map #(g/translate % point-a)
                        (concat (assign-open panes (:open a))
                                (assign-open [t-clip] (xor-fill a b))
                                #_(assign-open (square/surrounding-panes (g/bounds b) (g/bounds clip) :all)
                                               (:open a))))))
           (filter (comp square/has-area? g/bounds))))))

(comment
  (assert (= (g/clip-with (g/as-polygon (rect/rect 0 0 10 10))
                          (g/as-polygon (rect/rect 12 12 10 10)))
             (gp/polygon2 [])))

  (map #(g/translate % (gv/vec2 200 150))
       (filter square/has-area?
               (square/surrounding-panes (rect/rect 200 150 400 300)
                                         (rect/rect 0 0 200 150) :row)))

  (poly-exclusion base-shape (random-rect))
  (poly-exclusion (random-rect) (random-rect)))

;; still finding gaps, need to include clip + deal with zero width/height?
(defn add-split-shapes [shapes s]
  (let [isec? (fn [x] (g/intersect-shape (g/bounds s) (g/bounds x)))
        intersections (filter isec? shapes)
        disjoint (remove isec? shapes)
        additions (if (= (count intersections) 1)
                    (poly-exclusion (first intersections) s)
                    (mapcat #(poly-exclusion % s) intersections))]
    (concat disjoint additions)))

(comment
  (add-split-shapes
   (add-split-shapes [base-shape] (first (assign-open [(rect/rect (rv 0.25 0.25) (rv 0.75 0.75))] true)))
   (first (assign-open [(rect/rect (rv 0 0) (rv 0.5 0.5))] true))))

(defn fill-shape [{:keys [open] :as shape}]
  (vary-meta shape assoc :fill
             (if open "cornflowerblue" "goldenrod")))

(defn random-additions [n]
  (->> random-rect
       repeatedly
       (filter big-enough?)
       (take n)))

(defn shapes []
  (let [additions (assign-open [(rect/rect (rv 0.25 0.25) (rv 0.75 0.75))
                                (rect/rect (rv 0 0) (rv 0.5 0.5))] true)]
    [(svg/group {} (map fill-shape (reduce add-split-shapes [base-shape] additions)))
     (svg/group {:fill "#F00"}
                (mapcat (fn [{:keys [points]}] (map #(svg/circle % 2) points)) additions))]))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (apply list (shapes))))

(sketch/definition negative-overlap
  {:created-at "2022-01-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :negative-overlap)
              "sketch-host"))

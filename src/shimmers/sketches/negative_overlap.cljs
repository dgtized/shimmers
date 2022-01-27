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
    (assoc (rect/rect (rv x y) (rv w h))
           :open true)))

(defn big-enough? [rect]
  (let [{[w h] :size} rect]
    (and (> w 20) (> h 20))))

(def base-shape
  (assoc (rect/rect 0 0 width height) :open false))

(defn assign-open [shapes open]
  (map #(assoc % :open open) shapes))

(defn xor-fill [{a :open} {b :open}]
  (= (bit-xor a b) 1))

(defn rect= [{:keys [p size]} {pb :p sizeb :size}]
  (and (tm/delta= p pb) (tm/delta= size sizeb)))

(defn translated-panes [{pa :p :as a} b]
  (->> (square/surrounding-panes (g/translate a (tm/- pa))
                                 (g/translate b (tm/- pa))
                                 (square/row-major a))
       (map #(g/translate % pa))
       (filter (comp square/has-area? g/bounds))))

;; for now this is removing the clip each time
(defn rect-exclusion [a b]
  (let [clip (g/clip-with (g/as-polygon a) (g/as-polygon b))]
    (if (empty? (:points clip)) ;; no intersection between pair
      [a]
      (let [clip (g/bounds clip)]
        (cond (rect= clip b) ;; b is contained by a
              (concat (assign-open (translated-panes a b) (:open a))
                      (assign-open [clip] (xor-fill a b)))
              (rect= clip a) ;; a is contained by b
              ;; This is probably causing a bug if b also clips other shapes?
              (concat (assign-open (translated-panes b a) (:open b))
                      (assign-open [clip] (xor-fill a b)))
              :else ;; partial overlap
              (concat (assign-open (translated-panes a clip) (:open a))
                      (assign-open [clip] (xor-fill a b))))))))

(def example (assign-open [(rect/rect (rv 0.25 0.25) (rv 0.75 0.75))
                           (rect/rect (rv 0 0) (rv 0.5 0.5))] true))

(comment
  (assert (= (g/clip-with (g/as-polygon (rect/rect 0 0 10 10))
                          (g/as-polygon (rect/rect 12 12 10 10)))
             (gp/polygon2 [])))

  (map #(g/translate % (gv/vec2 200 150))
       (filter square/has-area?
               (square/surrounding-panes (rect/rect 200 150 400 300)
                                         (rect/rect 0 0 200 150) :row)))

  (rect-exclusion base-shape (random-rect))
  (rect-exclusion (random-rect) (random-rect)))

;; FIXME: ignoring any remainder of shape that did not intersect anything
(defn add-split-shapes [shapes s]
  (let [isec? (fn [x] (g/intersect-shape (g/bounds s) (g/bounds x)))
        intersections (filter isec? shapes)
        disjoint (remove isec? shapes)]
    (concat disjoint (mapcat #(rect-exclusion % s) intersections))))

(comment
  (add-split-shapes (add-split-shapes [base-shape] (first example))
                    (second example)))

(defn fill-shape [{:keys [open] :as shape}]
  (vary-meta shape assoc :fill
             (if open "cornflowerblue" "goldenrod")))

(defn random-additions [n]
  (->> random-rect
       repeatedly
       (filter big-enough?)
       (take n)))

(defn shapes []
  (let [additions (random-additions 4)]
    [(svg/group {} (map fill-shape (reduce add-split-shapes [base-shape] additions)))
     (svg/group {:fill "#F00"}
                (mapcat (fn [r] (map #(svg/circle % 2) (g/vertices r))) additions))]))

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

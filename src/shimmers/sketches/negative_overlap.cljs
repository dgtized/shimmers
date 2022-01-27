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
  (println a " exclude " b)
  (let [clip (g/clip-with a b)]
    (if (empty? (:points clip)) ;; no intersection between pair
      [a]
      (->> (cond (same-shape? clip b) ;; b is contained by a
                 (do (println "b inside a")
                     (concat (assign-open (square/surrounding-panes (g/bounds a) (g/bounds b) :all)
                                          (:open a))
                             #_(assign-open [clip] (xor-fill a b))))
                 (same-shape? clip a) ;; a is contained by b
                 (do (println "a inside b")
                     (concat (assign-open (square/surrounding-panes (g/bounds b) (g/bounds a) :all)
                                          (:open b))
                             #_(assign-open [clip] (xor-fill a b))))
                 :else ;; partial overlap
                 (do (println "partial" clip)
                     (concat (assign-open (square/surrounding-panes (g/bounds a) (g/bounds clip) :all)
                                          (:open a))
                             #_(assign-open [clip] (xor-fill a b))
                             #_(assign-open (square/surrounding-panes (g/bounds b) (g/bounds clip) :all)
                                            (:open a)))))
           (filter (comp square/has-area? g/bounds))))))

(comment
  (assert (= (g/clip-with (g/as-polygon (rect/rect 0 0 10 10))
                          (g/as-polygon (rect/rect 12 12 10 10)))
             (gp/polygon2 [])))

  (poly-exclusion base-shape (random-rect))
  (poly-exclusion (random-rect) (random-rect)))

;; still finding gaps, need to include clip + deal with zero width/height?
(defn add-split-shapes [shapes s]
  (let [isec? (fn [x] (g/intersect-shape (g/bounds s) (g/bounds x)))
        intersections (filter isec? shapes)
        disjoint (remove isec? shapes)]
    (.log js/console "shape:" s (count shapes))
    (.log js/console "isec:" intersections)
    (.log js/console "disj:" disjoint)
    (let [exclusions (map #(poly-exclusion % s) intersections)
          r (concat disjoint (apply concat exclusions) [s])]
      (println (map count exclusions) (count r))
      r)))

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
    [(svg/group {} (map fill-shape (split-shapes base-shape additions)))
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

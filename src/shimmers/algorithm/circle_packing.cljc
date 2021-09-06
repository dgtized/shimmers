(ns shimmers.algorithm.circle-packing
  (:require [shimmers.math.equations :as eq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.spatialtree :as spatialtree]))

;; Considering implementing
;; https://paytonturnage.com/writing/circle-packing-quad-trees/ for performance
;; instead of the max-radius per packing iteration.

(defn intersects
  [spacing
   {p1 :p r1 :r}
   {p2 :p r2 :r :as c2}]
  (let [dist-sqr (eq/sqr (+ r1 r2 spacing))]
    (when (< (geom/dist-squared p1 p2) dist-sqr)
      c2)))

(defn add-circle [quadtree {:keys [bounds radius spacing]} max-radius]
  (let [p (geom/random-point-inside bounds)
        candidate (gc/circle p radius)
        search-radius (+ max-radius radius (* 2 spacing))
        near (spatialtree/select-with-circle quadtree p search-radius)]
    (when-not (some (partial intersects spacing candidate) near)
      candidate)))

(defn circle-pack
  "Pack a `bounds` object with `candidate` circles that do not intersect any
  circles in `circles`.

  Circles can be of specified `radius` and `spacing` between. This function is
  intentionally re-entrant, allowing up to `candidate` circles to be added on
  each invocation."
  [circles {:keys [bounds radius candidates] :as rules}]
  (let [quadtree (reduce (fn [t {p :p :as circle}]
                           (geom/add-point t p circle))
                         (spatialtree/quadtree (geom/bounds bounds))
                         circles)
        max-radius (or (:r (apply (partial max-key :r) circles))
                       radius)]
    (loop [i 0 circles circles tree quadtree]
      (if (>= i candidates)
        circles
        (if-let [{p :p :as circle} (add-circle tree rules max-radius)]
          (recur (inc i)
                 (conj circles circle)
                 (geom/add-point tree p circle))
          (recur (inc i) circles tree))))))

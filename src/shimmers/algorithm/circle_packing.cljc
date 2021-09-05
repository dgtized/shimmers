(ns shimmers.algorithm.circle-packing
  (:require [shimmers.math.equations :as eq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]))

(defn intersects
  [spacing
   {p1 :p r1 :r}
   {p2 :p r2 :r :as c2}]
  (let [dist-sqr (eq/sqr (+ r1 r2 spacing))]
    (when (< (geom/dist-squared p1 p2) dist-sqr)
      c2)))

(defn add-circle [circles {:keys [bounds radius spacing]}]
  (let [p (geom/random-point-inside bounds)
        near circles ;; todo optimize
        candidate (gc/circle p radius)]
    (when-not (some (partial intersects spacing candidate) near)
      candidate)))

(defn circle-pack
  "Pack a `bounds` object with `candidate` circles that do not intersect any
  circles in `circles`.

  Circles can be of specified `radius` and `spacing` between. This function is
  intentionally re-entrant, allowing up to `candidate` circles to be added on
  each invocation."
  [circles {:keys [candidates] :as rules}]
  (loop [i 0 circles circles]
    (if (>= i candidates)
      circles
      (if-let [circle (add-circle circles rules)]
        (recur (inc i)
               (conj circles circle))
        (recur (inc i) circles)))))

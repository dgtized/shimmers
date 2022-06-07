(ns shimmers.algorithm.circle-packing
  (:require
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.math.geometry :as geometry]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]))

;; Considering implementing
;; https://paytonturnage.com/writing/circle-packing-quad-trees/ for performance
;; instead of the max-radius per packing iteration.
;; Also see: https://observablehq.com/@llb4ll/k-nearest-neighbor-search-using-d3-quadtrees

;; TODO: variations from
;; https://sighack.com/post/circle-packing-using-stochastic-search

(defn add-circle-to-tree
  [tree {p :p :as circle}]
  (g/add-point tree p circle))

(defn add-circle [quadtree {:keys [bounds radius spacing]}]
  (let [p (g/random-point-inside bounds)
        candidate (gc/circle p radius)]
    (when (geometry/contains-circle? bounds candidate)
      (if-let [near (saq/closest-circle quadtree candidate)]
        (when (> (saq/circle-overlap near candidate) spacing)
          candidate)
        candidate))))

(defn circle-pack
  "Pack a `bounds` object with `candidate` circles that do not intersect any
  circles in `circles`.

  Circles can be of specified `radius` and `spacing` between. This function is
  intentionally re-entrant, allowing up to `candidate` circles to be added on
  each invocation."
  [circles {:keys [bounds candidates] :as rules}]
  (let [quadtree (reduce add-circle-to-tree
                         (saq/circletree (g/bounds bounds))
                         circles)]
    (loop [i 0 circles circles tree quadtree]
      (if (>= i candidates)
        circles
        (if-let [circle (add-circle tree rules)]
          (recur (inc i)
                 (conj circles circle)
                 (add-circle-to-tree tree circle))
          (recur (inc i) circles tree))))))

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

(defn replace-circle-at [tree {p :p :as c}]
  (saq/replace-point tree p c))

(defn add-circle-at [tree {p :p :as c}]
  (saq/add-point tree p c))

;; Can spacing stay in the generation phase, any smaller circles will
;; automatically fit, and that could remove the bounds check?
(defn add-circle [quadtree {:keys [bounds radius spacing]}]
  (let [p (g/random-point-inside bounds)
        candidate (gc/circle p radius)]
    (when (geometry/contains-circle? bounds candidate)
      (if-let [near (saq/closest-circle quadtree candidate)]
        (when (> (saq/circle-overlap near candidate) spacing)
          candidate)
        candidate))))

;; FIXME: re-use circletree if provided?
;; pass in circle generation function?
;; return new circles so that we can detect if no circles were added and guard against that lock?
(defn circle-pack
  "Pack a `bounds` object with `candidate` circles that do not intersect any
  circles in `circles`.

  Circles can be of specified `radius` and `spacing` between. This function is
  intentionally re-entrant, allowing up to `candidate` circles to be added on
  each invocation."
  [circles {:keys [bounds candidates] :as rules}]
  (let [quadtree (reduce add-circle-at
                         (saq/circletree (g/bounds bounds))
                         circles)]
    (loop [i 0 circles circles tree quadtree]
      (if (>= i candidates)
        circles
        (if-let [circle (add-circle tree rules)]
          (recur (inc i)
                 (conj circles circle)
                 (add-circle-at tree circle))
          (recur (inc i) circles tree))))))

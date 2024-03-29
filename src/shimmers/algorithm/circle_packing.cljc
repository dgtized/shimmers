(ns shimmers.algorithm.circle-packing
  (:require
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.math.geometry :as geometry]
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
(defn legal-candidate
  [circletree
   {:keys [bounds gen-circle spacing] :or {spacing 0}}]
  (when-let [candidate (gen-circle)]
    (when (geometry/contains-circle? bounds candidate)
      (if-let [near (saq/closest-circle circletree candidate)]
        (when (> (saq/circle-overlap near candidate) spacing)
          candidate)
        candidate))))

(defn pack-candidates
  [circletree n rules]
  (loop [i 0 tree circletree circles []]
    (if (>= i n)
      [circles tree]
      (if-let [circle (legal-candidate tree rules)]
        (recur (inc i)
               (add-circle-at tree circle)
               (conj circles circle))
        (recur (inc i) tree circles)))))

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
                         circles)
        [circles' _] (pack-candidates quadtree candidates rules)]
    (into circles circles')))

(defn add-circles [circletree legal-candidate n]
  (loop [i 0 tree circletree]
    (if (>= i n)
      tree
      (if-let [circle (legal-candidate tree)]
        (recur (inc i)
               (saq/add-point tree (:p circle) circle))
        (recur i tree)))))

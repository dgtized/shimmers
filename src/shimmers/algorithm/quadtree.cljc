(ns shimmers.algorithm.quadtree
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.spatialtree :as spatialtree]))

(defn add-point
  "Safer add-point that throws an out of bounds exception instead of returning a nil tree"
  [tree point data]
  (or (g/add-point tree point data)
      (throw (ex-info "Unable to add-point to tree, possibly out of bounds?"
                      {:bounds (g/bounds tree)
                       :point point
                       :data data}))))

(defn replace-point
  "Replace a point in tree, recovering if point is missing, and using safer add-point."
  [tree point data]
  (let [deleted (g/delete-point tree point)]
    (add-point (or deleted tree) point data)))

;; Problems:
;;
;; 1. Varying the size of the circles to lookup exposes that isec? is checking the
;; bounds of the quad, but circles can cross outside of a quad.
;; 2. Wholly contained cursor or cursor containing a circle are not showing as hits
;; 3. Really what I want is k-nearest neighboring circles?
;; 4. What happens if point data is heterogeneous (rects & circles)
;; 5. Performance?

;; Again, re-read https://paytonturnage.com/writing/circle-packing-quad-trees/

(defn lazy-select-quad
  "This is very similar to spatialtree/lazy-select-with but overlap? receives the
  quadtree and not the underlying point so as to allow overlap comparison with
  the point data. Not clear if actually performant yet though?"
  [isec? overlap? queue]
  (lazy-seq
   (let [[q & r] queue]
     (if (and q (isec? (g/bounds q)))
       (let [children (filter identity (spatialtree/get-children q))
             p (g/get-point q)]
         (if (seq children)
           (lazy-select-quad isec? overlap? (concat children r))
           (if (and p (overlap? q))
             (cons (g/get-point-data q) (lazy-select-quad isec? overlap? r))
             (when (seq r) (lazy-select-quad isec? overlap? r)))))
       (when (seq r) (lazy-select-quad isec? overlap? r))))))

(defn- further-than-best?
  [tree point d]
  (let [{:keys [p size]} (g/bounds tree)]
    ;; loop over each axis so should work for vec2 and vec3
    (some (fn [axis]
            (let [v (nth point axis)]
              (or (< v (- (nth p axis) d))
                  (> v (+ (nth p axis) (nth size axis) d)))))
          (range (count size)))))

;; translated from http://bl.ocks.org/patricksurry/6478178
(defn nearest-neighbor-node
  "Given a `spatialtree` and a `point`, return the nearest leaf node to `point`.

  Use `g/get-point` and `g/get-point-data` to fetch neighbor from the node."
  ([tree point]
   (let [max-dist (apply + (:size (g/bounds tree)))]
     (:node (nearest-neighbor-node tree point
                                   {:distance max-dist :node tree}))))
  ([tree point {:keys [distance] :as best}]
   (if (further-than-best? tree point distance)
     best
     (let [best' (or (when-let [node-point (g/get-point tree)]
                       (let [d' (g/dist point node-point)]
                         (when (< d' distance)
                           {:distance d' :node tree})))
                     best)]
       (reduce (fn [better child]
                 (if child
                   (nearest-neighbor-node child point better)
                   better))
               best' (spatialtree/get-children tree))))))

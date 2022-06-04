(ns shimmers.algorithm.quadtree
  (:require
   [shimmers.math.geometry :as geometry]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.math.core :as tm]
   #?(:clj [clojure.data.priority-map :as priority]
      :cljs [tailrecursion.priority-map :as priority])))

(defn largest-circle [rect circles]
  (->> circles
       (keep (fn [c]
               (when (and c (g/contains-point? rect (:p c)))
                 c)))
       (apply max-key :r)))

;; https://paytonturnage.com/writing/circle-packing-quad-trees/
(deftype MutableCircleTreeNode
    #?(:clj
       [^:unsynchronized-mutable rect
        ^:unsynchronized-mutable children
        ^:unsynchronized-mutable circle
        meta]
       :cljs
       [^:mutable rect
        ^:mutable children
        ^:mutable circle
        meta])

  g/ISpatialTree
  (add-point [_ p d]
    (when (g/contains-point? rect p)
      (spatialtree/add-point* _ p d)
      _))
  (delete-point [_ p] (spatialtree/delete-point* _ p))
  (get-point [_] (:p circle))
  (get-point-data [_] circle)

  g/IClear
  (clear*
    [_] (MutableCircleTreeNode. rect nil nil meta))
  (clear!
    [_]
    (set! children nil)
    (set! circle nil)
    _)

  spatialtree/PTreeOps
  (child-index-for-point
    [_ [px py]]
    (let [{[x y] :p [w h] :size} rect]
      (if (< px (+ x (* 0.5 w)))
        (if (< py (+ y (* 0.5 h))) 0 2)
        (if (< py (+ y (* 0.5 h))) 1 3))))
  (child-for-point
    [_ p]
    (when children
      (children (spatialtree/child-index-for-point _ p))))
  (make-child-for-point
    [_ p d add?]
    (let [idx (spatialtree/child-index-for-point _ p)]
      (or (children idx)
          (let [{[x y] :p [w h] :size} rect
                cx (if (> (bit-and idx 1) 0) (+ x (* 0.5 w)) x)
                cy (if (> (bit-and idx 2) 0) (+ y (* 0.5 h)) y)
                r  (rect/rect cx cy (* 0.5 w) (* 0.5 h))
                c  (MutableCircleTreeNode.
                    r nil
                    (if add? d (largest-circle r [d circle]))
                    nil)]
            (spatialtree/set-child _ idx c)
            c))))
  (split-node
    [_]
    (set! children [nil nil nil nil])
    ;; split-node is called *before* make-child-for-point so don't override
    ;; (set! circle nil)
    _)
  (get-children [_] children)
  (set-child [_ i c]
    (set! children (assoc children i c))
    (set! circle (largest-circle rect (mapv g/get-point-data (filter some? children))))
    _)
  (set-children [_ c] (set! children c) _)
  (set-point [_ _ d]
    (set! circle d)
    _)

  g/IBounds
  (bounds [_] rect)

  #?@(:clj
      [clojure.lang.IObj
       (meta [_] meta)
       (withMeta
        [_ meta']
        (if (identical? meta' meta)
          _
          (MutableCircleTreeNode. rect children circle meta')))]
      :cljs
      [IMeta
       (-meta [_] meta)
       IWithMeta
       (-with-meta
        [_ meta']
        (if (identical? meta' meta)
          _
          (MutableCircleTreeNode. rect children circle meta')))])

  ;; not working in Javascript?
  Object
  (toString
    [_]
    (str "#shimmers.algorithm.quadtree.MutableCircleTreeNode"
         "{:rect " (pr-str rect)
         " :children " (pr-str children)
         " :circle " (pr-str circle)
         "}"))

  #?@(:cljs
      [IPrintWithWriter
       (-pr-writer
        [_ writer opts]
        (pr-seq-writer
         [(symbol "#shimmers.algorithm.quadtree.MutableCircleTreeNode")
          {:rect rect :children children :circle circle}]
         writer
         opts))]))

(defn circletree
  "Create a new circletree root node with the given XY position & dimensions."
  ([{[x y] :p [w h] :size}]
   (circletree x y w h))
  ([[x y] size]
   (let [[w h] (if (number? size) [size size] size)]
     (circletree x y w h)))
  ([x y size]
   (circletree x y size size))
  ([x y w h]
   (MutableCircleTreeNode. (rect/rect x y w h) nil nil nil)))

(comment
  (let [tree (->> [(gc/circle 3 2 3) (gc/circle 8 4 4) (gc/circle 2 3 5)]
                  (reduce (fn [t {:keys [p] :as c}] (g/add-point t p c))
                          (circletree 0 0 10 10)))]
    (spatialtree/select-with-shape tree (rect/rect 10))))

(defn circle-overlap [a b]
  (- (g/dist (:p a) (:p b)) (:r a) (:r b)))

(defn eligable-subtree [query n]
  (when-let [largest (and (some? n) (g/get-point-data n))]
    (let [distance-to-bounds
          (tm/mag (geometry/manhattan-to-rectangle (g/bounds n) (:p query)))
          close-as-possible
          (- distance-to-bounds (:r query) (:r largest))]
      (< close-as-possible (circle-overlap query largest)))))

(defn closest-circle
  [tree query]
  (loop [queue [tree] nearest nil]
    (if (empty? queue)
      nearest
      (let [node (peek queue)
            largest (g/get-point-data node)
            closest (if (< (circle-overlap query largest)
                           (circle-overlap query nearest))
                      largest
                      nearest)]
        (if-let [children (seq (spatialtree/get-children node))]
          (let [eligable (filter (partial eligable-subtree query) children)]
            (recur (into (pop queue) eligable) closest))
          (recur (pop queue) closest))))))

;; Helpers
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

(defn distance-to-neighbor [node point]
  (if (seq (spatialtree/get-children node))
    (tm/mag (geometry/manhattan-to-rectangle (g/bounds node) point))
    (g/dist point (g/get-point node))))

;; Vague translation of
;; https://observablehq.com/@llb4ll/k-nearest-neighbor-search-using-d3-quadtrees
(defn k-nearest-neighbors
  ([tree k point]
   (k-nearest-neighbors (priority/priority-map tree 0) k point []))
  ([best-queue k point results]
   (if (or (empty? best-queue) (>= (count results) k))
     results
     (let [[node _] (peek best-queue)]
       (if-let [children (seq (spatialtree/get-children node))]
         (recur (reduce (fn [q n]
                          (if n
                            (assoc q n (distance-to-neighbor n point))
                            q))
                        (pop best-queue)
                        children)
                k point results)
         (recur (pop best-queue) k point (conj results node)))))))

;; Some other references:
;; https://pvigier.github.io/2019/08/04/quadtree-collision-detection.html

(ns shimmers.algorithm.polygon-detection
  (:require
   [clojure.math :as math]
   [clojure.set :as set]
   [loom.graph :as lg]
   [shimmers.common.sequence :as cs]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn vertices-per-isec
  "Calculate the set of vertices for each segment from the intersections."
  [intersections]
  (->> (for [{:keys [isec segments]} intersections
             seg segments]
         {seg #{isec}})
       (apply (partial merge-with set/union))))

;; WIP just trying to get basic output here. I think problems with directed vs undirected graph?
(defn intersections->edges [isecs]
  (apply set/union
         (for [[{[a b] :points} vertices] (vertices-per-isec isecs)]
           (let [ordered (sort-by (fn [p] (g/dist a p)) (conj vertices a b))]
             (->> ordered
                  dedupe ;; sometimes a or b is already in points
                  (partition 2 1)
                  ;; ensure edges are always low pt -> high pt
                  (map (fn [v] (sort v)))
                  set)))))

(defn edges->graph [edges]
  (reduce (fn [g [a b]] (lg/add-edges g [a b (g/dist a b)]))
          (lg/weighted-graph) edges))

(defn atan2 [[x y]]
  (math/atan2 y x))

;; g/angle-between is clockwise angle between with range 0 - 2π.
(defn small-angle-between
  "Returns the absolute relative angle-between vectors `p` and `q`.
  The range is 0 to π."
  [p q]
  (abs (- (atan2 p) (atan2 q))))

;; TODO: optimize these a bit?
(defn clockwise-point
  "Given a point `from` going to `vertex`, return the clockwise point in
  `outbound`."
  [from vertex outbound]
  (when (seq outbound)
    (if-let [points (seq (remove #{from} outbound))]
      (let [from-vertex (tm/- from vertex)]
        (->> points
             (map (fn [v] (let [p (tm/- v vertex)]
                           [[(g/angle-between p from-vertex)
                             (/ 1.0 (tm/mag-squared p))]
                            v])))
             (sort-by first)
             last
             second))
      from)))

(defn counter-clockwise-point
  "Given a point `from` going to `vertex`, return the counter-clockwise point in
  `outbound`."
  [from vertex outbound]
  (when (seq outbound)
    (if-let [points (seq (remove #{from} outbound))]
      (let [from-vertex (tm/- from vertex)]
        (->> points
             (map (fn [v] (let [p (tm/- v vertex)]
                           [[(g/angle-between p from-vertex)
                             (tm/mag-squared p)]
                            v])))
             (sort-by first)
             first
             second))
      from)))

(defn find-cycle
  "Find a cycle in `graph` from `start` with more than 2 vertices.

  Use `select-point` to select a candidate successor from the given vertex given
  the cycle thus far, the current vertex and all unvisited successor nodes other
  than the `start` node."
  [graph select-point start]
  (loop [cycle [] vertex start]
    (let [cycle' (conj cycle vertex)
          candidates (remove (disj (set cycle') start) (lg/successors graph vertex))
          next-pt (select-point cycle vertex candidates)]
      (cond (empty? candidates)
            []
            ;; identical? *should* work but sometimes fails, so use tm/delta=
            (and (> (count cycle') 2) (tm/delta= next-pt start))
            cycle'
            :else
            (recur cycle' next-pt)))))

;; FIXME change to starting edge in a clockwise direction. Currently if
;; clockwise-starts gives a ccw point, it will detect a larger polygon with
;; internal edges.
(defn cycle-clockwise-from-edge [g start to]
  (find-cycle
   g
   (fn [cycle vertex points]
     (counter-clockwise-point (or (last cycle) start)
                              vertex points))
   to))

(defn closest-in [point points]
  (apply min-key (partial g/dist-squared point) points))

(defn closest-angle [point vertex points]
  (apply min-key (fn [q] (small-angle-between (tm/- point vertex) (tm/- q vertex)))
         points))

;; FIXME: occasionally this is finding the far edge when outside on a sliver triangles
(defn edge-face-near-point
  "Return the closest facing edge pair `p`, `q`

  `p` and `q` are ordered to ensure that point is oriented clockwise from the
  edge."
  [g point]
  (when-let [p (closest-in point (lg/nodes g))]
    (when-let [q (closest-angle point p (lg/successors g p))]
      (if (pos? (v/orientation p q point))
        [q p]
        [p q]))))

;; Better heuristics by selecting closest two points and finding closest
(defn edge-face-closest-point
  "Return the closest facing edge pair `p`, `q` in `g` from `point`.

  `p` and `q` are ordered to ensure that point is oriented clockwise from the
  edge."
  [g point]
  (when-let [[p q]
             (apply min-key
                    (fn [[p q]]
                      (g/dist-squared point
                                      (gu/closest-point-on-segment point p q)))
                    (lg/edges g))]
    (if (pos? (v/orientation p q point))
      [q p]
      [p q])))

;; Note that if initial point order is reversed, as if if point is outside of
;; the polygon it can construct the hull over the outer edge. Put differently,
;; following the counter-clockwise point on the outside gift wraps the polygon.
;; This doesn't always happen though so something is weird.
(defn cycle-near-point
  "Given a graph of points in a plane and a point, find the closest polygon near that point."
  [g point]
  (when-let [[p q] (edge-face-closest-point g point)]
    (cycle-clockwise-from-edge g p q)))

(defn polygon-around-point
  "Given a graph `g`, and a `point`, find the polygon surrounding the point."
  [g point]
  (when-let [cycle (cycle-near-point g point)]
    (let [polygon (gp/polygon2 cycle)]
      (when (g/contains-point? polygon point)
        polygon))))

;; Alternatives approaches with decomposition & finding circuits?
;; https://en.wikipedia.org/wiki/Polygon_partition#Partition_a_polygon_into_trapezoids
;; https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0230342
;; https://javascript.plainenglish.io/finding-simple-cycles-in-an-undirected-graph-a-javascript-approach-1fa84d2f3218
;; https://www.cs.tufts.edu/comp/150GA/homeworks/hw1/Johnson%2075.PDF
;; https://github.com/1123/johnson/blob/master/src/main/java/jgraphalgos/johnson/Johnson.java
;; http://www.cs.technion.ac.il/~itai/publications/Algorithms/min-circuit.pdf
;; https://web.ist.utl.pt/alfredo.ferreira/publications/12EPCG-PolygonDetection.pdf
;; (defn minimum-cycle-basis [g])

;; https://stackoverflow.com/questions/14505565/detect-if-a-set-of-points-in-an-array-that-are-the-vertices-of-a-complex-polygon
(defn shoelace-area [points]
  (->> (concat (rest points) (take 1 points))
       (map tm/cross points)
       (reduce + 0)
       (* 0.5)))

(defn clockwise-vertices
  "Force a clockwise ordering by sorting vertices around centroid.

  This only works if the polygon is convex. This is likely an artifact of
  `g/clip-with`."
  [convex-polygon]
  (let [centroid (g/centroid convex-polygon)]
    (->> convex-polygon
         g/vertices
         (sort-by (fn [v] (g/heading (tm/- v centroid)))))))

(defn clockwise-polygon?
  "return true if points in polygon are in a clockwise ordering."
  [points]
  (pos? (shoelace-area points)))

(defn disjoin-carefully [pending cycle-edges]
  (->> pending
       (remove (fn [[p q]]
                 (when (some (fn [[a b]]
                               (when (and (tm/delta= a p) (tm/delta= b q))
                                 true))
                             cycle-edges)
                   true)))
       set))

;; TODO: inline cycle-clockwise-from-edge and keep track of a single list of
;; seen edges
(defn simple-polygons
  "`graph` is a digraph of points, returns a set of polygons from each point cycles.

  They should be simple cycles without any internal edges/chords."
  [graph]
  (let [g (lg/digraph graph)
        all-edges (set (lg/edges g))]
    ;; FIXME: limit is a guard to avoid infinite loops if cycles cannot be
    ;; removed from pending. Something is causing cycles to get added repeatedly
    ;; instead of only once, so this safe-guards against.
    (loop [pending all-edges polygons [] limit (/ (count all-edges) 2)]
      (if (or (empty? pending) (zero? limit))
        (do (when (and (zero? limit) (> (count polygons) 2))
              (println "halted after " (count polygons) " polygons"))
            (map gp/polygon2 polygons))
        (let [edge (first pending)
              [p q] edge
              cycle (cycle-clockwise-from-edge g p q)]
          ;; FIXME: disjoin is sometimes not removing a cycle
          ;; probably due to floating point precision errors on match
          ;; (println i cycle (and (seq cycle) (clockwise-polygon? cycle)))
          (if (and (seq cycle) (clockwise-polygon? cycle))
            (let [cycle-edges (conj (partition 2 1 cycle) [(last cycle) (first cycle)])
                  pending' (reduce disj pending cycle-edges)]
              (recur (if-not (= (count pending') (count pending))
                       pending'
                       (disjoin-carefully pending cycle-edges))
                     (conj polygons cycle)
                     (dec limit)))
            (recur (disj pending edge) polygons
                   (dec limit))))))))

;; TODO: detect all simple chordless polygons in plane
;; polygon isomorphism?
;; detect if hull polygon by orientation of edges?
;; scale polygons towards center even if concave polygon without it looking weird?

(defn self-intersecting?
  "Returns the intersection coordinate of an edge that intersects another edge in a polygon.

  Excludes intersections at overlapping vertices, to ensure the intersection is
  at some point in the middle of an edge."
  [polygon]
  (->> polygon
       g/edges
       cs/all-pairs
       (some (fn [[[p0 q0] [p1 q1]]]
               (let [{type :type isec :p} (isec/intersect-line2-line2? p0 q0 p1 q1)]
                 (when (and (= :intersect type)
                            (not (tm/delta= isec p0))
                            (not (tm/delta= isec q0)))
                   isec))))))

(comment
  (g/heading (gv/vec2 -1 0))
  (g/heading (gv/vec2 -1 1))
  (g/heading (gv/vec2 0 1))
  (g/heading (gv/vec2 1 1))
  (g/heading (gv/vec2 1 0))
  (g/angle-between (gv/vec2 -1 1) (gv/vec2 0 1))
  (g/angle-between (gv/vec2 0 1) (gv/vec2 -1 1))
  (small-angle-between (gv/vec2 -1 1) (gv/vec2 0 1))
  (small-angle-between (gv/vec2 0 1) (gv/vec2 -1 1))

  (g/angle-between (gv/vec2 0 1) (gv/vec2 1 0))
  (g/angle-between (gv/vec2 1 0) (gv/vec2 0 1))
  (small-angle-between (gv/vec2 1 0) (gv/vec2 0 1))
  (small-angle-between (gv/vec2 0 1) (gv/vec2 1 0))

  (tm/cross (gv/vec2 5 3) (gv/vec2 5 4)))

(defn convex?
  [polygon]
  (let [vertices (g/vertices polygon)]
    (every? (fn [[a b c]] (< (g/angle-between (tm/- c b) (tm/- a b)) math/PI))
            (partition 3 1 (into vertices (take 2 vertices))))))

(defn concave?
  [polygon]
  (not (convex? polygon)))

;; Possibly worth looking into similar routines in the Java Topology Suite
;; https://github.com/locationtech/jts, and https://github.com/bjornharrtell/jsts


;; extracted from thi.ng.geom.polygon to address bugs
;; http://alienryderflex.com/polygon_inset/

;; The problem here I *think* is if a corner has a small segment like: a -> b --
;; c -> d, where b -- c is small, and a->b and c->d will intersect prior to b--c
;; if they are inset, resulting in a self intersection, and a ccw triangle
;; containing the remaining b--c edge. Presumably there can be more then one
;; edge between self-intersection points.

;; The problem here is that the polygon needs to be split into two new polygons
;; that share the self-intersection point.
(defn- inset-corner
  [prev curr next d]
  (let [[dx1 dy1 :as d1] (tm/- curr prev)
        [dx2 dy2 :as d2] (tm/- next curr)
        d1 (tm/mag d1)
        d2 (tm/mag d2)]
    (if-not (or (tm/delta= 0.0 d1) (tm/delta= 0.0 d2))
      (let [i1 (tm/* (gv/vec2 dy1 (- dx1)) (/ d d1))
            i2 (tm/* (gv/vec2 dy2 (- dx2)) (/ d d2))
            c1 (tm/+ curr i1)
            c2 (tm/+ curr i2)
            prev' (tm/+ prev i1)
            next' (tm/+ next i2)]
        (if (tm/delta= c1 c2)
          c1
          ;; IDEA: check if c1 is still clockwise from c2 otherwise handle self-intersect?
          (let [isec (isec/intersect-line2-line2? prev' c1 c2 next')]
            ;; FIXME: c1 is not right, but better than error on nil vector
            (if (= (:type isec) :parallel)
              c1
              (get isec :p)))))
      curr)))

;; references:
;; https://stackoverflow.com/questions/1109536/an-algorithm-for-inflating-deflating-offsetting-buffering-polygons

(defn inset-polygon
  "For CW polygons, use positive distance to inset or negative to outset.
  For CCW polygons, use opposite."
  [{:keys [points]} d]
  (->> points
       cs/triplet-cycle
       (mapv (fn [[p c n]] (inset-corner p c n (- d))))
       gp/polygon2))

(defn point-on-line?
  ([p q point] (point-on-line? p q point tm/*eps*))
  ([p q point epsilon]
   (when (tm/delta= (+ (g/dist p point) (g/dist point q))
                    (g/dist p q)
                    epsilon)
     point)))

(defn split-self-intersection
  "Recursively splits a polygon into a sequence of polygons on each
  self-intersection point."
  [polygon]
  (if-let [isec (self-intersecting? polygon)]
    (loop [edges (g/edges polygon)
           a [] b [] in-split-polygon false]
      (let [[[p q] & remaining] edges]
        (cond (empty? edges)
              (concat (split-self-intersection (gp/polygon2 a))
                      (split-self-intersection (gp/polygon2 b)))
              (point-on-line? p q isec)
              (if-not in-split-polygon
                (recur remaining
                       (conj a p)
                       (conj b isec)
                       true)
                (recur remaining
                       (conj a isec)
                       (conj b p)
                       false))
              :else
              (if in-split-polygon
                (recur remaining
                       a
                       (conj b p)
                       true)
                (recur remaining
                       (conj a p)
                       b
                       false)))))
    [polygon]))

;; FIXME: improve distinct filter & see if can apply this in one pass with below
(defn self-intersections
  "Returns all self-intersecting coordinate of an edge that intersects another edge in a polygon.

  Excludes intersections at overlapping vertices, to ensure the intersection is
  at some point in the middle of an edge."
  [polygon]
  (->> polygon
       g/edges
       cs/all-pairs
       (keep (fn [[[p0 q0] [p1 q1]]]
               (let [{type :type isec :p} (isec/intersect-line2-line2? p0 q0 p1 q1)]
                 (when (and (= :intersect type)
                            (not (tm/delta= isec p0))
                            (not (tm/delta= isec q0)))
                   isec))))
       distinct))

(defn self-intersection-polygons
  [polygon]
  (if-let [intersections (self-intersections polygon)]
    (->> polygon
         g/edges
         (reduce
          (fn [g [a b]]
            (let [line-isecs (keep (fn [p] (when (point-on-line? a b p) p))
                                   intersections)]
              (apply lg/add-edges g
                     (if (empty? line-isecs)
                       [[a b (g/dist a b)]]
                       (let [isecs (sort-by #(g/dist a %) line-isecs)]
                         (for [[p q] (partition 2 1 (concat [a] isecs [b]))]
                           [p q (g/dist p q)]))))))
          (lg/weighted-graph))
         simple-polygons)
    [polygon]))

(ns shimmers.algorithm.polygon-detection
  (:require
   [loom.graph :as lg]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn atan2 [[x y]]
  (Math/atan2 y x))

;; g/angle-between is clockwise angle between with range 0 - 2π.
(defn small-angle-between
  "Returns the absolute relative angle-between vectors `p` and `q`.
  The range is 0 to π."
  [p q]
  (Math/abs (- (atan2 p) (atan2 q))))

;; TODO: optimize these a bit?
(defn clockwise-point
  "Given a point `from` going to `vertex`, return the clockwise point in
  `outbound`."
  [from vertex outbound]
  (when (seq outbound)
    (if-let [points (seq (remove (hash-set from) outbound))]
      (let [from-vertex (tm/- from vertex)]
        (apply (partial max-key
                        (fn [v] (let [p (tm/- v vertex)]
                                 [(g/angle-between p from-vertex)
                                  (/ 1.0 (tm/mag p))])))
               points))
      from)))

(defn counter-clockwise-point
  "Given a point `from` going to `vertex`, return the counter-clockwise point in
  `outbound`."
  [from vertex outbound]
  (when (seq outbound)
    (if-let [points (seq (remove (hash-set from) outbound))]
      (let [from-vertex (tm/- from vertex)]
        (apply (partial min-key
                        (fn [v] (let [p (tm/- v vertex)]
                                 [(- (g/angle-between p from-vertex))
                                  (- (tm/mag p))])))
               points))
      from)))

(defn cycle-clockwise-from-edge [g start to]
  ;; FIXME change to starting edge in a clockwise direction. Currently if
  ;; clockwise-starts gives a ccw point, it will detect a larger polygon with
  ;; internal edges.
  (loop [cycle [start] vertex to]
    (let [previous-pt (or (last cycle) start)
          candidates (remove (disj (set cycle) start) (lg/successors g vertex))
          next-pt (counter-clockwise-point previous-pt vertex candidates)
          cycle' (conj cycle vertex)]
      (cond (empty? candidates)
            []
            ;; FIXME: Why are points occasionally not identical?
            (and (> (count cycle') 2) (tm/delta= next-pt start))
            cycle'
            :else
            (recur cycle' next-pt)))))

(defn closest-in [point points]
  (apply min-key (partial g/dist-squared point) points))

(defn closest-angle [point vertex points]
  (apply min-key (fn [q] (small-angle-between (tm/- point vertex) (tm/- q vertex)))
         points))

(defn face-edge-near-point
  "Return the closest facing edge pair `p`, `q`

  `p` and `q` are ordered to ensure that point is oriented clockwise from the
  edge."
  [g point]
  (when-let [p (closest-in point (lg/nodes g))]
    (when-let [q (closest-angle point p (lg/successors g p))]
      (if (> (v/orientation p q point) 0)
        [q p]
        [p q]))))

(defn polygon-near-point
  "Given a graph of points in a plane and a point, find the closest polygon around that point."
  [g point]
  (when-let [[p q] (face-edge-near-point g point)]
    (cycle-clockwise-from-edge g p q)))

;; TODO: detect all simple chordless polygons in plane
;; polygon isomorphism?
;; detect if hull polygon by orientation of edges?
;; scale polygons towards center even if concave polygon without it looking weird?

;; Note that if initial point order is reversed, as if if point is outside of
;; the polygon it can construct the hull over the outer edge. Put differently,
;; following the counter-clockwise point on the outside gift wraps the polygon.

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

  (tm/cross (gv/vec2 5 3) (gv/vec2 5 4))
  )

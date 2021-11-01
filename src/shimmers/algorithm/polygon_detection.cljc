(ns shimmers.algorithm.polygon-detection
  (:require
   [loom.graph :as lg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn atan2 [[x y]]
  (Math/atan2 y x))

(defn norm-angle [theta]
  (if (< theta 0) (+ theta tm/TWO_PI) theta))

(defn small-angle-between [a b]
  (Math/abs (- (g/heading a) (g/heading b))))

;; From a starting location with surrounding candidate nodes
;; Return the counter-clockwise candidate of the pair with the smallest angle between
;; Note: heuristic, doesn't guarentee path started from here is start of a clockwise polygon
(defn clockwise-starts [start candidates]
  (let [ordered (sort-by (fn [p] (g/heading (tm/- p start))) candidates)
        [a b]
        (->> (vec ordered)
             (cons (last ordered))
             (partition 2 1)
             (sort-by (fn [[a b]] [(small-angle-between (tm/- a start) (tm/- b start)) a b]))
             first)]
    (if (> (g/angle-between (tm/- a start) (tm/- b start))
           (g/angle-between (tm/- b start) (tm/- a start)))
      b a)))

;; TODO: optimize these a bit?
(defn clockwise-point
  "Given a point `from` going to `vertex`, return the clockwise point in
  `outbound`."
  [from vertex outbound]
  (let [pts (sort-by (fn [v]
                       (let [p (tm/- v vertex)]
                         [(g/heading p) (tm/mag p)]))
                     outbound)
        angle (g/heading (tm/- from vertex))
        after (drop-while (fn [v] (>= angle (g/heading (tm/- v vertex)))) pts)]
    (if (seq after)
      (first after)
      (first pts))))

(defn counter-clockwise-point
  "Given a point `from` going to `vertex`, return the counter-clockwise point in
  `outbound`."
  [from vertex outbound]
  (let [pts (sort-by (fn [v]
                       (let [p (tm/- v vertex)]
                         [(g/heading p) (tm/mag p)]))
                     outbound)
        angle (g/heading (tm/- from vertex))
        before (take-while (fn [v] (< (g/heading (tm/- v vertex)) angle)) pts)]
    (if (seq before)
      (last before)
      (last pts))))

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

(defn cycle-clockwise [g start]
  (let [point (clockwise-starts start (lg/successors g start))
        a (cycle-clockwise-from-edge g start point)
        b (cycle-clockwise-from-edge g point start)]
    ;; return the smallest non-empty cycle
    (if (and (seq a) (< (count a) (count b)))
      a b)))

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

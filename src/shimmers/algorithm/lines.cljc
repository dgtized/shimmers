(ns shimmers.algorithm.lines
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn points->lines [points]
  (map gl/line2 (partition 2 1 points)))

(defn lines->points [lines]
  (->> lines
       (mapcat g/vertices)
       dedupe))

(defn segment-at
  ([line] (segment-at line 0.5))
  ([{[p q] :points} d]
   [(gl/line2 (tm/mix p q 0) (tm/mix p q d))
    (gl/line2 (tm/mix p q d) (tm/mix p q 1.0))]))

(defn segmented
  [{[p q] :points} n]
  {:pre [(pos-int? n)]}
  (for [[a b] (partition 2 1 (tm/norm-range n))]
    (gl/line2 (tm/mix p q a) (tm/mix p q b))))

;; https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
(defn perpendicular-distance [[x1 y1] [x2 y2] [x0 y0]]
  (let [x21 (- x2 x1)
        y21 (- y2 y1)]
    (/ (Math/abs (- (* x21 (- y1 y0))
                    (* (- x1 x0) y21)))
       (Math/sqrt (+ (* x21 x21) (* y21 y21))))))

(defn max-perpendicular-distance [points]
  (let [a (first points)
        b (last points)]
    (loop [i 1 index 0 max-dist 0]
      (if (>= i (dec (count points)))
        [index max-dist]
        (let [pt (nth points i)
              d (perpendicular-distance a b pt)]
          (if (> d max-dist)
            (recur (inc i) i d)
            (recur (inc i) index max-dist)))))))

;; https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
(defn ramer-douglas-peucker [points epsilon]
  (let [[index max-dist] (max-perpendicular-distance points)]
    (if (> max-dist epsilon)
      (lazy-cat (ramer-douglas-peucker (take index points) epsilon)
                (ramer-douglas-peucker (drop index points) epsilon))
      [(first points) (last points)])))

(defn indexed-line-strip [points]
  (assoc (gl/linestrip2 points)
         :arc-index (gu/arc-length-index points)))

(defn simplify-line
  "Remove any points that deviate further than `epsilon` distance from the
  perpindicular of the subsection of the line."
  [{:keys [points]} epsilon]
  (indexed-line-strip (ramer-douglas-peucker points epsilon)))

(defn mix-line
  "Sample two paths at even intervals and return a line-strip with a linear
  interpolation of `factor` between each of the two corresponding points."
  [{a :points arc-index-a :arc-index}
   {b :points arc-index-b :arc-index}
   factor]
  (let [arc-index-a (or arc-index-a (gu/arc-length-index a))
        arc-index-b (or arc-index-b (gu/arc-length-index b))
        samples (max (count a) (count b))]
    (indexed-line-strip
     (for [t (tm/norm-range (dec samples))]
       (tm/mix (gu/point-at t a arc-index-a)
               (gu/point-at t b arc-index-b)
               factor)))))

(defn dampen
  "Mix each point in a `path` by `factor` with the linear path between the
  beginning and end of `path`."
  [{path :points} factor]
  (gl/linestrip2
   (let [a (first path)
         b (last path)]
     (mapv (fn [pt t] (tm/mix pt (tm/mix a b t) factor))
           path
           (tm/norm-range (dec (count path)))))))

(defn points-between
  "Given a sequence of `points`, and relative offsets `t0` < `t1`, return all
  points that proportionally lie between `t0` and `t1`. The domain of `t0` and
  `t1` is [0.0..1.0], represents the percentage distance along the total arc
  length of the original points."
  ([points t0 t1]
   (points-between points t0 t1 (gu/arc-length-index points)))
  ([points t0 t1 arc-index]
   {:pre [(<= t0 t1)]}
   (let [arc-length (last arc-index)]
     (->> (map (fn [p arc] [p (/ arc arc-length)]) points arc-index)
          (drop-while (fn [[_ t]] (< t t0)))
          (take-while (fn [[_ t]] (<= t t1)))
          (map first)))))

(defn connect
  "Connect two linestrips `a` and `b` at offset `t` with a single line.

  `t` is a proprotional offset along the arc-length of the linestrip ranged [0..1]."
  [[{a :points arc-index-a :arc-index}
    {b :points arc-index-b :arc-index}] t]
  (let [arc-index-a (or arc-index-a (gu/arc-length-index a))
        arc-index-b (or arc-index-b (gu/arc-length-index b))]
    (gl/line2 (gu/point-at t a arc-index-a)
              (gu/point-at t b arc-index-b))))

(defn box-between
  "Connect two linestrips `a` and `b` between the offsets `t0` and `t1` to create a polygon.

  The polygon contains the connecting points at the specific offsets, as well as
  every intervening point along `a` and `b` between the offsets `t0` and `t1`.

  `t0` and `t1` are ranged from [0..1] and represent proportional offsets along
  the arc-length of each linestrip. The assumption is that `a` and `b` do not
  cross eachother."
  [[{a :points arc-index-a :arc-index}
    {b :points arc-index-b :arc-index}]
   t0 t1]
  (let [arc-index-a (or arc-index-a (gu/arc-length-index a))
        arc-index-b (or arc-index-b (gu/arc-length-index b))
        b0-b1 (points-between b t0 t1 arc-index-b)
        a0-a1 (points-between a t0 t1 arc-index-a)]
    (gp/polygon2 (concat [(gu/point-at t0 a arc-index-a)
                          (gu/point-at t0 b arc-index-b)]
                         b0-b1
                         [(gu/point-at t1 b arc-index-b)
                          (gu/point-at t1 a arc-index-a)]
                         (reverse a0-a1)))))

(defn clip-line
  "Clip a `line` into set of line segments contained by the `polygon`."
  [line polygon]
  (let [[p q] (:points line)
        points (->> polygon
                    g/edges
                    (keep (fn [[e0 e1]]
                            (let [isec (isec/intersect-line2-line2? p q e0 e1)]
                              (when (= (:type isec) :intersect)
                                (:p isec)))))
                    (sort-by (partial g/dist-squared p))
                    ;; if line clips at a corner, remove duplicates
                    ;; might have difficulty with floating point coordinate boundaries
                    dedupe)]
    (mapv gl/line2
          (cond (empty? points)
                (if (and (g/contains-point? polygon p)
                         (g/contains-point? polygon q))
                  [[p q]]
                  [])
                (even? (count points))
                (partition 2 2 points)
                :else
                (if (g/contains-point? polygon p)
                  (let [[a & xs] points]
                    (concat (if (tm/delta= p a)
                              []
                              [[p a]])
                            (partition 2 2 xs)))
                  (let [z (last points)]
                    (concat (partition 2 2 (butlast points))
                            (if (tm/delta= q z)
                              []
                              [[z q]]))))))))

;; TODO: handle coincident line segments
;; at least two cases,
;; a) trim line to non-coincident segments,
;; b) split line into each non-coincident segments
(defn find-paired-intersections [edges {[pl ql] :points}]
  (let [isecs (->> edges
                   (keep (fn [edge]
                           (let [[pe qe] edge
                                 isec (isec/intersect-line2-line2? pl ql pe qe)]
                             (when (and (= (:type isec) :intersect)
                                        (not (tm/delta= qe (:p isec))))
                               {:edge edge :p (:p isec)}))))
                   (sort-by (fn [{:keys [p]}] (g/dist-squared pl p))))
        pairs (partition 2 2 (map :p isecs))]
    (for [isec isecs]
      (if-let [opposite (some (fn [[p q]] (cond (tm/delta= p (:p isec)) q
                                               (tm/delta= q (:p isec)) p))
                              pairs)]
        (assoc isec :pair opposite)
        isec))))

(comment (let [a (gv/vec2 0 0)
               b (gv/vec2 5 0)
               c (gv/vec2 10 0)
               d (gv/vec2 20 0)]
           [(isec/intersect-line2-line2? a b a c)
            (isec/intersect-line2-line2? a b b c)
            (isec/intersect-line2-line2? a d b c)]))

;; https://stackoverflow.com/a/5533807/34450
;; See also https://www.inf.usi.ch/hormann/papers/Greiner.1998.ECO.pdf for generalized
;; FIXME: not handling internal coincident edges on concave polygons
(defn cut-polygon
  "Cut a polygon with a line, returning the set of polygons from each side of the
  line."
  [polygon line]
  (let [edges (g/edges polygon)
        isecs (find-paired-intersections edges line)]
    (if (< (count isecs) 2)
      [(g/as-polygon polygon)]
      (loop [[edge & remaining] edges active [] shapes []]
        (if (nil? edge)
          (->> (conj shapes active)
               (mapv dedupe)
               (filter (fn [points] (> (count points) 2)))
               (mapv gp/polygon2))
          (let [p (first edge)
                current-polygon (conj active p)]
            (if-let [isec (some (fn [{iedge :edge :as isec}] (when (= iedge edge) isec)) isecs)]
              (let [{cut-p :p cut-q :pair} isec]
                (if-let [resume (some (fn [s] (when (tm/delta= (last s) cut-q) s)) shapes)]
                  (recur remaining
                         (conj resume cut-p)
                         (conj (remove #{resume} shapes)
                               (conj current-polygon cut-p)))
                  (recur remaining
                         [cut-p]
                         (conj shapes (conj current-polygon cut-p)))))
              (recur remaining current-polygon shapes))))))))

(comment
  (let [poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10] [8 4] [2 4] [2 10] [0 10])
        line (gl/line2 [2 0] [2 10])]
    {:pairs (find-paired-intersections (g/edges poly) line)
     :cuts (cut-polygon poly line)})

  (let [poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10] [8 4] [2 4] [2 10] [0 10])]
    [(cut-polygon poly (gl/line2 [2 0] [2 10]))
     (cut-polygon poly (gl/line2 [0 4] [10 4]))]))

;; TODO: join-polygon
;; simple case is for coincident lines, complex is if edges cut eachother.

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

(defn remove-coincident-segments
  "Remove sequential coincident segments from a polygon.

  TODO: generalize for linestrip?"
  [poly]
  (let [points (g/vertices poly)]
    (if (<= (count points) 3)
      poly
      (->> (concat (take-last 1 points) points (take 1 points))
           (partition 3 1)
           (keep (fn [[a b c]]
                   (let [{:keys [type]} (isec/intersect-line2-line2? a b b c)]
                     (when (not= type :coincident)
                       b))))
           gp/polygon2))))

;; TODO: handle coincident line segments
;; at least two cases,
;; a) trim line to non-coincident segments,
;; b) split line into each non-coincident segments
(defn find-paired-intersections [polygon {[pl ql] :points :as line}]
  (let [isecs
        (->> (g/edges polygon)
             (mapcat (fn [edge]
                       (let [[pe qe] edge
                             {:keys [type p]} (isec/intersect-line2-line2? pe qe pl ql)]
                         (cond (and (= type :intersect)
                                    (not (tm/delta= qe p)))
                               [{:edge edge :p p}]
                               ;; awkward coincident case, simplify
                               (#{:coincident-no-intersect :coincident} type)
                               (for [{[sp sq] :points} (clip-line line polygon)
                                     :let [{:keys [p]} (isec/intersect-line2-line2? pe qe sp sq)]
                                     :when (not (tm/delta= qe p))]
                                 {:edge edge :p p})
                               :else
                               []))))
             (sort-by (fn [{:keys [p]}] (g/dist-squared pl p))))

        pairs (partition 2 2 (map :p isecs))]
    (for [isec isecs
          :let [opposite (some (fn [[a b]]
                                 (cond (tm/delta= a (:p isec)) b
                                       (tm/delta= b (:p isec)) a))
                               pairs)]
          :when opposite
          ]
      (assoc isec :pair opposite))))

(comment
  (let [a (gv/vec2 10 0)
        b (gv/vec2 20 0)
        c (gv/vec2 30 0)
        d (gv/vec2 40 0)]
    {:ab-ac (isec/intersect-line2-line2? a b a c)
     :ac-ab (isec/intersect-line2-line2? a c a b)
     :ba-ca (isec/intersect-line2-line2? b a c a)
     :ca-ba (isec/intersect-line2-line2? c a b a)
     :ab-bc (isec/intersect-line2-line2? a b b c)
     :bc-ab (isec/intersect-line2-line2? b c a b)
     :ad-bc (isec/intersect-line2-line2? a d b c)
     ;; coincident-no-intersect because neither b or c match an endpoint of a-d
     ;; but are wholly contained and coincidental
     :bc-ad (isec/intersect-line2-line2? b c a d)
     :ac-bd (isec/intersect-line2-line2? a c b d)}))

(defn intersection-with-edge [isecs edge]
  (some (fn [{iedge :edge :as isec}]
          (when (= iedge edge)
            isec))
        isecs))

(defn resume-shape-at-point [shapes cut-q]
  (some (fn [shape]
          (when (tm/delta= (last shape) cut-q)
            shape))
        shapes))

;; https://stackoverflow.com/a/5533807/34450
;; See also https://www.inf.usi.ch/hormann/papers/Greiner.1998.ECO.pdf for generalized
;; FIXME: not handling internal coincident edges on concave polygons
;; looks like it's not joining the coincident segments into a single line segment
(defn cut-polygon
  "Cut a polygon with a line, returning the set of polygons from each side of the
  line."
  [polygon line]
  (let [cleaned-polygons
        (fn [shapes]
          (->> shapes
               (mapv dedupe) ;; TODO: can this get handled with remove-coincident?
               (filter (fn [points] (> (count points) 2)))
               (mapv (fn [points]
                       (->> (if (tm/delta= (first points) (last points))
                              (butlast points)
                              points)
                            gp/polygon2
                            remove-coincident-segments)))))
        isecs (find-paired-intersections polygon line)]
    (if (< (count isecs) 2)
      [(g/as-polygon polygon)]
      (loop [[edge & remaining] (g/edges polygon) active [] shapes []]
        (if (nil? edge)
          (cleaned-polygons (conj shapes active))
          (let [p (first edge)]
            (if-let [isec (intersection-with-edge isecs edge)]
              (let [{cut-p :p cut-q :pair} isec
                    current-polygon (conj active p cut-p)]
                (if-let [existing-shape (resume-shape-at-point shapes cut-q)]
                  ;; resume appending to polygon from crossback pair
                  (recur remaining
                         (conj existing-shape cut-p)
                         (conj (remove #{existing-shape} shapes)
                               current-polygon))
                  ;; start new polygon
                  (recur remaining
                         [cut-p]
                         (conj shapes current-polygon))))
              ;; append point to active shape
              (recur remaining (conj active p) shapes))))))))

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

(defn overlapping-polygon? [a b]
  (some? (some (partial g/contains-point? a) (g/vertices b))))

(defn polygon-intersections [a b]
  (for [a-edge (g/edges a)
        b-edge (g/edges b)
        :let [[ap aq] a-edge
              [bp bq] b-edge
              isec (isec/intersect-line2-line2? ap aq bp bq)]
        :when (contains? #{:coincident :intersect} (:type isec))]
    [a-edge b-edge isec]))

(defn join-polygons [a b]
  (if-not (overlapping-polygon? a b)
    nil
    nil))

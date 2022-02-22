(ns shimmers.sketches.shapes-and-patterns
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn circle-row [v row-height]
  (let [diameter row-height
        cols (tm/floor (/ width diameter))
        r (* 0.5 diameter)]
    (for [u (tm/norm-range cols)]
      (gc/circle (rv (+ u (/ r width)) (+ v (/ r height))) (* 0.95 r)))))

(defn triangle-row [v row-height]
  (let [base row-height
        cols (tm/floor (/ width base))
        up (dr/chance 0.5)
        triangle (-> (gt/equilateral2 (gv/vec2 0.0 0.0)
                                      (gv/vec2 (* -0.8 base) 0.0))
                     g/center
                     (g/rotate (if up 0 (* 0.5 eq/TAU))))]
    (for [u (tm/norm-range cols)]
      (g/translate triangle (rv (+ u (* 0.5 (/ base width)))
                                (+ v (* 0.5 (/ base height))))))))

;; fixme, align the top/bottom of each triangle?
(defn updown-row [v row-height]
  (let [base row-height
        cols (tm/floor (/ width (* 0.9 base)))

        triangle1 (-> (gt/equilateral2 (gv/vec2 0.0 0.0)
                                       (gv/vec2 (* -0.75 base) 0.0))
                      g/center)
        triangle2 (-> (gt/equilateral2 (gv/vec2 (* -0.75 base) 0.0)
                                       (gv/vec2 0.0 0.0))
                      g/center)
        freq (dr/random-int 2 5)]
    (into (for [[idx u] (map-indexed vector (tm/norm-range cols))]
            (-> (if (zero? (mod idx freq)) triangle1 triangle2)
                (g/translate (rv (+ u (* 0.5 (/ base width)))
                                 (+ v (* 0.5 (/ base height)))))))
          [(gl/line2 (rv 0.0 v) (rv 1.0 v))
           (gl/line2 (rv 0.0 (+ v (/ base height)))
                     (rv 1.0 (+ v (/ base height))))])))

(defn box-row [v row-height]
  (let [r (* 0.5 row-height)
        rw (/ r width)
        rh (/ r height)
        base (* 0.5 row-height)
        cols (tm/floor (/ width row-height))
        square (rect/rect base)]
    (apply concat
           (for [u (tm/norm-range cols)]
             [(g/translate square
                           (rv (+ u (* 0.25 rw))
                               (+ v (* 0.25 rh))))
              (g/translate square
                           (rv (+ u (* 0.5 rw))
                               (+ v (* 0.5 rh))))]))))

(defn rulers [v row-height]
  (for [p (cs/midsection (tm/norm-range (dr/rand-nth [1 2 3 5])))
        :let [t (/ p row-height)]]
    (gl/line2 (rv 0.0 (+ v t)) (rv 1.0 (+ v t)))))

(defn zig-zag [v row-height]
  (let [cols (tm/floor (/ width row-height (dr/weighted {1 4 2 2 3 1})))]
    (csvg/path (into [[:M (rv 0.0 v)]]
                     (for [[idx p] (map-indexed vector (tm/norm-range cols))
                           :let [offset (if (zero? (mod idx 2))
                                          (/ (* 0.1 row-height) height)
                                          (/ (* 0.9 row-height) height))]]
                       [:L (rv p (+ v offset))])))))

(defn sawtooth [v row-height]
  (let [cols (tm/floor (/ width row-height (dr/weighted {1 6 2 2 3 1})))
        rh (* 0.8 row-height)
        rw (* 1 (/ width cols))]
    (csvg/path (into [[:M (rv 0.0 v)]]
                     (for [[idx p] (map-indexed vector (tm/norm-range cols))
                           :let [zero (zero? (mod idx 2))
                                 offset (if zero
                                          (/ (* 0.1 row-height) height)
                                          (/ (* 0.9 row-height) height))]]
                       [:A [rw rh] 0 0 (if zero 0 1) (rv p (+ v offset))])))))

;; Consider using markov chain transition probabilities between each row type?
(defn shapes [rows]
  (let [ranges (dr/var-range rows)
        heights (map - (rest ranges) ranges)]
    (for [[v gap] (map vector ranges heights)
          :let [row-height (* gap height)]
          :when (> row-height (* 0.02 height))]
      ((dr/weighted {#(svg/group {}) 1
                     #(circle-row v row-height) 3
                     #(triangle-row v row-height) 3
                     #(updown-row v row-height) 3
                     #(box-row v row-height) 3
                     #(rulers v row-height) 2
                     #(sawtooth v row-height) 2
                     #(zig-zag v row-height) 2})))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
            (apply list (shapes (dr/rand-nth [17 23])))))

(sketch/definition shapes-and-patterns
  {:created-at "2022-02-19"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :shapes-and-patterns)
              "sketch-host"))

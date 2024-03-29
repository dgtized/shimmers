(ns shimmers.sketches.shapes-and-patterns
  (:require
   [shimmers.algorithm.markov :as markov]
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
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defmulti svg-row :type)

(defmethod svg-row :blank [_]
  (csvg/group {}))

(defmethod svg-row :circle [{:keys [v row-height]}]
  (let [diameter row-height
        cols (tm/floor (/ width diameter))
        r (* 0.5 diameter)]
    (for [u (tm/norm-range cols)]
      (gc/circle (rv (+ u (/ r width)) (+ v (/ r height))) (* 0.95 r)))))

(defn triangle [up v row-height]
  (let [base row-height
        cols (tm/floor (/ width base))
        triangle (-> (gt/equilateral2 (gv/vec2 0.0 0.0)
                                      (gv/vec2 (* -0.8 base) 0.0))
                     g/center
                     (g/rotate (if up 0 (* 0.5 eq/TAU))))]
    (for [u (tm/norm-range cols)]
      (g/translate triangle (rv (+ u (* 0.5 (/ base width)))
                                (+ v (* 0.5 (/ base height))))))))

(defmethod svg-row :triangle-up [{:keys [v row-height]}]
  (triangle true v row-height))

(defmethod svg-row :triangle-down [{:keys [v row-height]}]
  (triangle false v row-height))

;; fixme, align the top/bottom of each triangle?
(defmethod svg-row :updown [{:keys [v row-height]}]
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

(defmethod svg-row :box [{:keys [v row-height]}]
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

(defmethod svg-row :rulers [{:keys [v row-height]}]
  (for [p (cs/midsection (tm/norm-range (inc (dr/rand-nth [1 2 3 5]))))
        :let [t (/ p row-height)]]
    (gl/line2 (rv 0.0 (+ v t)) (rv 1.0 (+ v t)))))

(defmethod svg-row :zig-zag [{:keys [v row-height]}]
  (let [cols (tm/floor (/ width row-height (dr/weighted {1 4 2 2 3 1})))]
    (csvg/path (into [[:M (rv 0.0 v)]]
                     (for [[idx p] (map-indexed vector (tm/norm-range cols))
                           :let [offset (if (zero? (mod idx 2))
                                          (/ (* 0.1 row-height) height)
                                          (/ (* 0.9 row-height) height))]]
                       [:L (rv p (+ v offset))])))))

(defmethod svg-row :sawtooth [{:keys [v row-height]}]
  (let [cols (tm/floor (/ width row-height (dr/weighted {1 6 2 2})))
        rh (* 0.8 row-height)
        rw (* 1 (/ width cols))]
    (csvg/path (into [[:M (rv 0.0 v)]]
                     (for [[idx p] (map-indexed vector (tm/norm-range cols))
                           :let [zero (zero? (mod idx 2))
                                 offset (if zero
                                          (/ (* 0.1 row-height) height)
                                          (/ (* 0.9 row-height) height))]]
                       [:A [rw rh] 0 0 (if zero 0 1) (rv p (+ v offset))])))))

(def states [:blank :circle :triangle-up :triangle-down :updown :box :rulers :sawtooth :zig-zag])

(defn chain []
  (let [base (markov/all-transitions states 1)]
    (markov/combine base
                    (markov/learn [:circle :circle :box] 1 1)
                    (markov/learn [:circle :sawtooth :updown] 1 1)
                    (markov/learn [:circle :box] 1 1)
                    (markov/learn [:triangle-up :triangle-down] 1 1)
                    (markov/learn [:triangle-down :triangle-up] 1 1)
                    (markov/learn [:updown :triangle-up] 1 1)
                    (markov/learn [:updown :triangle-down] 1 1))))

(def distribution
  {:blank 1
   :circle 4
   :triangle-up 2
   :triangle-down 2
   :updown 4
   :box 4
   :rulers 2
   :sawtooth 2
   :zig-zag 2})

;; Consider using markov chain transition probabilities between each row type?
(defn shapes [rows]
  (let [ranges (dr/var-range rows)
        heights (map - (rest ranges) ranges)
        ;; types (take rows (markov/predict (chain)))
        types (repeatedly rows #(dr/weighted distribution))]
    (for [[v gap type] (map vector ranges heights types)
          :let [row-height (* gap height)]
          :when (> row-height (* 0.02 height))]
      (svg-row {:type type :v v :row-height row-height}))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (dr/rand-nth [17 23]))))

(sketch/definition shapes-and-patterns
  {:created-at "2022-02-19"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :shapes-and-patterns)))

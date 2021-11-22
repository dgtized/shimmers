(ns shimmers.sketches.displacements-inbetween
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))
(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-line [a b controls scale]
  (let [perpendicular (tm/normalize (g/normal (tm/- b a)) scale)]
    (-> (concat [a]
                (for [t (cs/midsection (tm/norm-range (inc controls)))]
                  (tm/+ (tm/mix a b t)
                        (tm/* perpendicular (dr/random -1 1))))
                [b])
        bezier/auto-spline2
        (g/sample-uniform (* 0.01 height) true)
        gl/linestrip2)))

(def spacing-divisions
  {5 1
   7 2
   11 3
   13 4
   17 4
   19 3
   23 2
   29 1})

(defn base-lines []
  (let [simplify (fn [line] (lines/simplify-line line (* 0.0002 width)))
        angle (if (dr/chance 0.1)
                (dr/random -0.3 0.3)
                (dr/random -0.15 0.15))
        a (-> (make-line (r 0.1 0.1) (r 0.1 0.9) 2 (* 0.08 width))
              (g/rotate (dr/random -0.05 0.1))
              simplify
              (vary-meta assoc :stroke-width 2.0))
        b (-> (make-line (r 0.5 0.0) (r 0.5 1.0) 3 (* 0.12 width))
              (g/rotate (dr/random -0.05 0.05))
              simplify
              (vary-meta assoc :stroke-width 2.0))
        c (-> (make-line (r 0.9 0.1) (r 0.9 0.9) 2 (* 0.08 width))
              (g/rotate angle)
              simplify
              (vary-meta assoc :stroke-width 2.0))
        [n1 n2] (repeatedly 2 #(dr/weighted (dissoc spacing-divisions 5 7)))]
    (swap! defo assoc :base [angle n1 n2])
    (concat [a]
            (for [t (tm/norm-range n1)]
              (simplify (lines/mix-line a b t)))
            [b]
            (for [t (tm/norm-range n2)]
              (simplify (lines/mix-line b c t)))
            [c])))

(defn connect [[{a :points arc-index-a :arc-index}
                {b :points arc-index-b :arc-index}] t]
  (let [arc-index-a (or arc-index-a (gu/arc-length-index a))
        arc-index-b (or arc-index-b (gu/arc-length-index b))]
    (gl/linestrip2 (gu/point-at t a arc-index-a)
                   (gu/point-at t b arc-index-b))))

(defn spaced
  [pair]
  (->> (dr/weighted spacing-divisions)
       tm/norm-range
       (mapv (fn [t] (connect pair t)))))

(defn box [[{a :points arc-index-a :arc-index}
            {b :points arc-index-b :arc-index}]
           t0 t1]
  (let [arc-index-a (or arc-index-a (gu/arc-length-index a))
        arc-index-b (or arc-index-b (gu/arc-length-index b))
        b0-b1 (lines/points-between b t0 t1 arc-index-b)
        a0-a1 (lines/points-between a t0 t1 arc-index-a)]
    (gp/polygon2 (concat [(gu/point-at t0 a arc-index-a)
                          (gu/point-at t0 b arc-index-b)]
                         b0-b1
                         [(gu/point-at t1 b arc-index-b)
                          (gu/point-at t1 a arc-index-a)]
                         (reverse a0-a1)))))

(defn box-strip [pair offsets]
  (for [[t0 t1] (partition 2 1 offsets)]
    (box pair t0 t1)))

;; TODO: improve palette selection
(defn color-strip [palette pair]
  (let [n (dr/weighted spacing-divisions)
        boxes (box-strip pair (tm/norm-range n))
        palette-seq (repeatedly (dr/rand-nth [2 3 4 5 7]) #(rand-nth palette))]
    (for [[i cell] (map-indexed vector boxes)]
      (vary-meta cell assoc :fill (nth palette-seq (mod i (count palette-seq)))))))

(defn lines [palette]
  (let [base (debug/time-it defo [:time :base-lines] (base-lines))
        lines (debug/time-it defo [:time :index]
                             (mapv (fn [strip] (assoc strip :arc-index
                                                     (gu/arc-length-index (:points strip))))
                                   base))
        pairs (partition 2 1 lines)
        sampling (dr/random-sample 0.5 pairs)]
    (concat (dr/map-random-sample (constantly 0.1)
                                  (fn [line] (vary-meta line assoc :stroke-width (dr/random 3 8)))
                                  lines)
            (debug/time-it defo [:time :spacing]
                           (dr/random-sample 0.85 (mapcat spaced sampling)))
            (debug/time-it defo [:time :color-strip]
                           (mapcat (partial color-strip palette)
                                   (dr/random-sample 0.05 pairs))))))

(defn fit-region
  "fit-all-into-bounds removes the meta attribs in copy, so add them back."
  [bounds shapes]
  (mapv (fn [shape fit] (with-meta fit (meta shape)))
        shapes
        (gu/fit-all-into-bounds bounds shapes)))

(defn scene []
  (let [screen (g/scale-size (rect/rect 0 0 width height) 0.95)
        palette (dr/shuffle ["maroon" "gold" "black" "white" "white"])
        shapes (debug/time-it defo [:time :generate] (fit-region screen (lines palette)))]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :stroke-opacity 0.8
               :stroke-width 1.0}
              (for [[i shape] (map-indexed vector shapes)]
                (vary-meta shape assoc :key (str "l" i))))))

(sketch/definition displacements-inbetween
  {:created-at "2021-11-13"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :displacements-inbetween)
              "sketch-host")
  (debug/mount defo))

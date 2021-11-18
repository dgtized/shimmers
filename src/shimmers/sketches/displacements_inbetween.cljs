(ns shimmers.sketches.displacements-inbetween
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
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
    (println [angle n1 n2])
    (concat [a]
            (for [t (tm/norm-range n1)]
              (simplify (lines/mix-line a b t)))
            [b]
            (for [t (tm/norm-range n2)]
              (simplify (lines/mix-line b c t)))
            [c])))

(defn connect [[a b] t]
  (gl/linestrip2 (g/point-at a t) (g/point-at b t)))

(defn spaced [pair]
  (mapv (fn [t] (connect pair t))
        (tm/norm-range (dr/weighted spacing-divisions))))

(defn random-connections [n pairs]
  (repeatedly n (fn [] (connect (dr/rand-nth pairs) (dr/random)))))

(defn p-if [p n]
  (if (< (dr/random-double) p)
    (dr/random n)
    0))

(defn box [[a b] t0 t1]
  (let [b0-b1 (lines/points-between (:points b) t0 t1)
        a0-a1 (lines/points-between (:points a) t0 t1)]
    (gp/polygon2 (concat [(g/point-at a t0)
                          (g/point-at b t0)]
                         b0-b1
                         [(g/point-at b t1)
                          (g/point-at a t1)]
                         (reverse a0-a1)))))

(defn box-strip [pair offsets]
  (for [[t0 t1] (partition 2 1 offsets)]
    (box pair t0 t1)))

;; TODO: either make a version doing an entire column or otherwise allow for
;; shorter length boxes. Also improve palette selection
(defn color-box [color pair]
  (let [t (dr/random 0.1 0.9)
        w (dr/random 0.05 0.2)
        t0 (- t (* 0.5 w))
        t1 (+ t (* 0.5 w))]
    (vary-meta (box pair t0 t1)
               assoc :fill color
               :fill-opacity 0.8)))

(defn color-strip [palette pair]
  (let [n (dr/weighted spacing-divisions)
        boxes (box-strip pair (tm/norm-range n))
        palette-seq (repeatedly (dr/rand-nth [2 3 4 5 7]) #(rand-nth palette))]
    (for [[i cell] (map-indexed vector boxes)]
      (vary-meta cell assoc :fill (nth palette-seq (mod i (count palette-seq)))))))

(comment (points-between (:points (make-line (r 0.0 0.0) (r 0.0 1.0) 2 3))
                         0.2 0.4))

(defn lines [palette]
  (let [lines (base-lines)
        pairs (partition 2 1 lines)
        sampling (dr/random-sample 0.5 pairs)]
    (concat (dr/map-random-sample (constantly 0.1)
                                  (fn [line] (vary-meta line assoc :stroke-width (dr/random 3 8)))
                                  lines)
            (dr/random-sample 0.85 (mapcat spaced sampling))
            #_(random-connections (int (p-if 0.3 100)) sampling)
            (mapcat (partial color-strip palette)
                    (dr/random-sample 0.05 pairs)))))

(defn fit-region
  "fit-all-into-bounds removes the meta attribs in copy, so add them back."
  [bounds shapes]
  (mapv (fn [shape fit] (with-meta fit (meta shape)))
        shapes
        (gu/fit-all-into-bounds bounds shapes)))

(defn scene []
  (let [screen (g/scale-size (rect/rect 0 0 width height) 0.95)
        palette (dr/shuffle ["maroon" "gold" "black" "white" "white"])
        shapes (fit-region screen (lines palette))]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :stroke-opacity 0.8
               :stroke-width 1.0}
              (for [[i shape] (map-indexed vector shapes)]
                (vary-meta shape assoc :key (str "l" i))))))

(defn page []
  [:div
   [:div.canvas-frame [scene]]
   [:p.center (view-sketch/generate :displacements-inbetween)]])

(sketch/definition displacements-inbetween
  {:created-at "2021-11-13"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "sketch-host"))

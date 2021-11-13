(ns shimmers.sketches.displacements-inbetween
  (:require
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
        (g/sample-uniform 8.0 true)
        gl/linestrip2)))

(defn mix-line [path-a path-b factor]
  (gl/linestrip2
   (let [samples (max (count (:points path-a))
                      (count (:points path-b)))]
     (for [t (tm/norm-range (dec samples))]
       (tm/mix (g/point-at path-a t) (g/point-at path-b t) factor)))))

(defn base-lines []
  (let [a (-> (make-line (r 0.1 0.1) (r 0.1 0.9) 2 (* 0.08 width))
              (g/rotate (dr/random -0.05 0.1))
              (vary-meta assoc :stroke-width 2.0))
        b (-> (make-line (r 0.5 0.0) (r 0.5 1.0) 3 (* 0.12 width))
              (g/rotate (dr/random -0.05 0.05))
              (vary-meta assoc :stroke-width 2.0))
        c (-> (make-line (r 0.9 0.1) (r 0.9 0.9) 2 (* 0.08 width))
              (g/rotate (if (dr/chance 0.1)
                          (dr/random -0.6 0.6)
                          (dr/random 0.05 -0.1)))
              (vary-meta assoc :stroke-width 2.0))
        [n1 n2] (repeatedly 2 #(dr/weighted {11 2
                                             13 2
                                             17 2
                                             23 1
                                             27 1}))]
    (concat [a]
            (for [t (tm/norm-range n1)]
              (mix-line a b t))
            [b]
            (for [t (tm/norm-range n2)]
              (mix-line b c t))
            [c])))

(defn connect [[a b] t]
  (gl/linestrip2 (g/point-at a t) (g/point-at b t)))

(def spacing-divisions
  {5 1
   7 2
   11 3
   13 4
   17 4
   19 3
   23 2
   29 1})

(defn spaced [pair]
  (mapv (fn [t] (connect pair t))
        (tm/norm-range (dr/weighted spacing-divisions))))

(defn random-connections [n pairs]
  (repeatedly n (fn [] (connect (dr/rand-nth pairs) (dr/random)))))

(defn p-if [p n]
  (if (< (dr/random-double) p)
    (dr/random n)
    0))

(defn points-between [points t0 t1]
  (let [arc-index (gu/arc-length-index points)
        arc-length (last arc-index)]
    (->> (map (fn [p arc] [p (/ arc arc-length)]) points arc-index)
         (drop-while (fn [[_ t]] (< t t0)))
         (take-while (fn [[_ t]] (< t t1)))
         (map first))))

(defn color-box [[a b]]
  (let [t0 (dr/random 0.75)
        t1 (+ t0 (dr/random (- 1.0 t0)))
        b0-b1 (points-between (:points b) t0 t1)
        a0-a1 (points-between (:points a) t0 t1)]
    (vary-meta (gp/polygon2 (concat [(g/point-at a t0)
                                     (g/point-at b t0)]
                                    b0-b1
                                    [(g/point-at b t1)
                                     (g/point-at a t1)]
                                    (reverse a0-a1)))
               assoc :fill (dr/rand-nth ["navy" "maroon"])
               :fill-opacity 0.8)))

(comment (points-between (:points (make-line (r 0.0 0.0) (r 0.0 1.0) 2 3))
                         0.2 0.4))

(def screen (g/scale (rect/rect 0 0 width height) 0.99))
(defn lines []
  (let [lines (base-lines)
        pairs (dr/random-sample 0.5 (partition 2 1 lines))]
    (concat (repeatedly 8 #(color-box (dr/rand-nth pairs)))
            (dr/map-random-sample (constantly 0.1)
                                  (fn [line] (vary-meta line assoc :stroke-width (dr/random 3 8)))
                                  lines)
            (dr/random-sample 0.85 (mapcat spaced pairs))
            (random-connections (int (p-if 0.3 100)) pairs))))

(defn fit-lines
  "fit-all-into-bounds removes the meta attribs in copy, so add them back."
  [lines]
  (mapv (fn [line fit] (with-meta fit (meta line)))
        lines
        (gu/fit-all-into-bounds screen lines)))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :stroke-width 0.8}
            (for [[i line] (map-indexed vector (fit-lines (lines)))]
              (vary-meta line assoc :key (str "l" i)))))

(defn page []
  [:div (scene)])

(defn ui-controls []
  [:div
   [:p.center (view-sketch/generate :displacements-inbetween)]])

(sketch/definition displacements-inbetween
  {:created-at "2021-11-13"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "canvas-host")
  (ctrl/mount ui-controls))

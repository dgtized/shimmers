(ns shimmers.sketches.ordered
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn distance-to-closest-point [shape p]
  (-> shape
      (g/closest-point p)
      (g/dist p)))

(defn closest-vertex-to-line [shape line]
  (apply min-key
         (partial distance-to-closest-point line)
         (g/vertices shape)))

(defn furthest-vertex-to-line [shape line]
  (apply max-key
         (partial distance-to-closest-point line)
         (g/vertices shape)))

(defn pick-side [bounds parent polygon last-cut]
  (let [angle (when last-cut (g/heading last-cut))]
    (dr/weighted
     (for [[p q] (concat (g/edges polygon)
                         (g/edges parent)
                         (g/edges bounds))
           :let [side (gl/line2 p q)]]
       [side
        (if (and last-cut (< (sm/radial-distance angle (g/heading side)) 0.1))
          0.2
          1)]))))

(defn cuts [polygon side n power]
  (let [lower (closest-vertex-to-line polygon side)
        upper (furthest-vertex-to-line polygon side)
        theta (g/heading side)
        len (max (g/width polygon) (g/height polygon))
        {[lp lq] :points} (gl/line2 (v/-polar lower len theta)
                                    (v/+polar lower len theta))
        {[up uq] :points} (gl/line2 (v/-polar upper len theta)
                                    (v/+polar upper len theta))]
    (for [pct (map (fn [x] (Math/pow x power))
                   (tm/norm-range n))]
      (gl/line2 (tm/mix lp up pct)
                (tm/mix lq uq pct)))))

(defn slice [polygon lines]
  (reduce (fn [polygons line]
            (mapcat (fn [poly] (filter #(> (count (:points %)) 0)
                                      (lines/cut-polygon poly line))) polygons))
          [polygon] lines))

(defn recurse-shapes [bounds parent shape last-side depth]
  (if (> depth 6)
    [shape]
    (let [side (pick-side bounds parent shape last-side)
          n-cuts (dr/weighted {0 (max 0 (* (- depth 2) 3))
                               1 8
                               2 3
                               3 3
                               4 2
                               5 1
                               6 1})
          power (dr/weighted {1 1
                              tm/PHI 1
                              2 1})]
      (mapcat (fn [s] (recurse-shapes bounds shape s side (inc depth)))
              (slice shape (cuts shape side n-cuts power))))))

(defn rectangle []
  (let [[pw ph] (dr/weighted {[0.5 0.75] 1
                              [0.33 0.66] 1})]
    (-> (rect/rect 0 0 (* pw width) (* ph height))
        g/center
        (g/rotate (* eq/TAU (dr/rand-nth [(/ 1 8) (/ 1 6) (/ 5 8) (/ 5 6)])))
        (g/translate (rv 0.5 0.5)))))

(defn n-gon [n]
  (fn []
    (-> (poly/regular-n-gon n (* 0.49 height))
        (g/rotate (* eq/TAU (dr/rand-nth [(/ 1 8) (/ 1 6) (/ 5 8) (/ 5 6)])))
        (g/translate (rv 0.5 0.5)))))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        s ((dr/rand-nth [rectangle (n-gon 5) (n-gon 6) (n-gon 8)]))
        shape (first (gu/fit-all-into-bounds bounds [s]))]
    (recurse-shapes bounds bounds shape nil 0)))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"}
     (shapes))))

(sketch/definition ordered
  {:created-at "2023-02-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :ordered)
              "sketch-host"))

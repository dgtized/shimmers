(ns shimmers.sketches.ordered
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.line :as gl]
   [shimmers.algorithm.lines :as lines]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]
   [shimmers.math.vector :as v]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn base-shape []
  (-> (rect/rect 0 0 (* 0.5 width) (* 0.75 height))
      g/center
      (g/rotate (* eq/TAU 0.13))
      (g/translate (rv 0.5 0.5))))

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

(defn pick-side [bounds polygon]
  (dr/weighted
   (for [[p q] (concat (g/edges polygon)
                       (g/edges bounds))]
     [(gl/line2 p q) 1])))

(defn cuts [polygon side n]
  (let [lower (closest-vertex-to-line polygon side)
        upper (furthest-vertex-to-line polygon side)
        theta (g/heading side)
        len (max (g/width polygon) (g/height polygon))
        {[lp lq] :points} (gl/line2 (v/-polar lower len theta)
                                    (v/+polar lower len theta))
        {[up uq] :points} (gl/line2 (v/-polar upper len theta)
                                    (v/+polar upper len theta))]
    (for [pct (map (fn [x] (Math/pow x (dr/weighted {1 1
                                                    tm/PHI 1
                                                    2 1})))
                   (tm/norm-range n))]
      (gl/line2 (tm/mix lp up pct)
                (tm/mix lq uq pct)))))

(defn slice [polygon lines]
  (reduce (fn [polygons line]
            (mapcat (fn [poly] (lines/cut-polygon poly line)) polygons))
          [polygon] lines))

(defn recurse-shapes [parent shape depth]
  (if (= depth 4)
    [shape]
    (let [n-cuts (dr/weighted {0 4
                               1 2
                               2 2
                               3 2
                               4 2
                               5 1
                               6 1
                               7 1})]
      (mapcat (fn [s] (recurse-shapes shape s (inc depth)))
              (slice shape (cuts shape
                                 (pick-side parent shape)
                                 n-cuts))))))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        shape (base-shape)]
    (recurse-shapes bounds shape 0)))

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

(ns shimmers.sketches.displacements-inbetween
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
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
  (let [samples (max (count (:points path-a))
                     (count (:points path-b)))]
    (for [t (tm/norm-range (dec samples))]
      (tm/mix (g/point-at path-a t) (g/point-at path-b t) factor))))

(defn scene []
  (let [a (make-line (r 0.1 0.1) (r 0.1 0.9) 2 (* 0.05 width))
        b (make-line (r 0.5 0.0) (r 0.5 1.0) 3 (* 0.1 width))
        c (make-line (r 0.9 0.1) (r 0.9 0.9) 2 (* 0.05 width))]
    (csvg/svg {:width width :height height}
              (svg/polyline (:points a)
                            {:key "a"
                             :stroke "black"
                             :stroke-width 2.0})
              (svg/polyline (:points b)
                            {:key "b"
                             :stroke "black"
                             :stroke-width 2.0})
              (svg/polyline (:points c)
                            {:key "c"
                             :stroke "black"
                             :stroke-width 2.0}))))

(defn page []
  [:div (scene)])

(sketch/definition displacements-inbetween
  {:created-at "2021-11-13"
   :type :svg}
  (ctrl/mount page "canvas-host"))

(ns shimmers.sketches.path-following
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

;; https://en.wikipedia.org/wiki/Visvalingam%E2%80%93Whyatt_algorithm

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

;; Experimenting with line simplification above, and dampening below.
(defn scene []
  (let [original (-> [(r 0.05 0.0)
                      (r 0.33 (dr/random -0.15 0.15))
                      (r 0.66 (dr/random -0.15 0.15))
                      (r 0.95 0.0)]
                     bezier/auto-spline2
                     (g/sample-uniform 10.0 true)
                     gl/linestrip2
                     (g/translate (r 0.0 0.5)))]
    (csvg/svg {:width width :height height}
      [(svg/polyline (:points original)
                     {:stroke "#efc020"
                      :stroke-width 10.0})]
      (for [v (range 0.0 1.0 0.1)]
        (svg/polyline (:points (g/translate (lines/dampen original v)
                                            (r 0.0 (+ 0.07 (* 0.3 v)))))
                      {:stroke "#3a3421"
                       :stroke-width (* 3.0 (- 1.0 v))}))
      (let [factors [1.0 2.0 4.0 8.0 12.0 14.0]]
        (for [[i eps] (map-indexed vector factors)]
          (svg/polyline (:points (g/translate (lines/simplify-line original eps)
                                              (r 0.0 (- -0.07 (* 0.05 i)))))
                        {:stroke "#da3b29"
                         :stroke-width (* 3.0 (- 1.0 (/ i (count factors))))}))))))

(sketch/definition path-following
  {:created-at "2021-11-12"
   :type :svg
   :tags #{:demo :deterministic}}
  (ctrl/mount (view-sketch/page-for scene :path-following)
              "sketch-host"))

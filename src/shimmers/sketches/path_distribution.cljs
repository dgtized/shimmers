(ns shimmers.sketches.path-distribution
  "Perturbations of displaced lines."
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]))

(defn displace-line
  "Generate a set of points on a curved line between p and q by picking a confused
  midpoint."
  [p q d]
  (let [curve (geometry/confused-midpoint p q d)]
    (geom/sample-uniform (bezier/auto-spline2 [p curve q]) 15 false)))

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn scene []
  (csvg/svg {:width width :height height}
            (concat [(svg/polyline [(r 0.1 0.1) (r 0.9 0.1)] {:stroke "black" :key "p0"})
                     (svg/polyline [(r 0.1 0.9) (r 0.9 0.9)] {:stroke "black" :key "p1"})]
                    (for [i (range 20)]
                      (svg/polyline (displace-line (r 0.1 0.5) (r 0.9 0.5) 0.3)
                                    {:stroke "black" :stroke-width 0.2 :key (str "line" i)})))))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210429
  (ctrl/mount page "svg-host"))

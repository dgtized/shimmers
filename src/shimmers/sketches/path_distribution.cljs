(ns shimmers.sketches.path-distribution
  "Perturbations of displaced lines."
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.bezier :as bezier]
            [shimmers.math.probability :as p]
            [thi.ng.math.core :as tm]))

(defn displace-line
  "For a given line p to q, find a random spot in the middle third, and pick a
  random point c in the circle around that spot and return a bezier curve
  running from p to q through c. "
  [p q d]
  (let [curve (->> (* d 0.5 (geom/dist p q))
                   (p/confusion-disk (tm/mix p q (tm/random 0.33 0.66)))
                   gv/vec2)]
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
                      (svg/polyline (displace-line (r 0.1 0.5) (r 0.9 0.5) 0.5)
                                    {:stroke "black" :stroke-width 0.2 :key (str "line" i)})))))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210429
  (ctrl/mount page "svg-host"))

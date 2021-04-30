(ns shimmers.sketches.path-distribution
  "Perturbations of displaced lines."
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn displace-line
  "Generate a set of points on a curved line between p and q by picking a confused
  midpoint."
  [p q d]
  (let [curve (geometry/confused-midpoint p q d)]
    (geom/sample-uniform (bezier/auto-spline2 [p curve q]) 15 true)))

;; https://stackoverflow.com/a/45701864/34450
(defn scaled-bisector [line d]
  (let [[p q] (geom/vertices line)
        midpoint (tm/mix p q 0.5)
        [x1 y1] (tm/* (tm/- p midpoint) d)
        [x2 y2] (tm/* (tm/- q midpoint) d)]
    (geom/translate (gl/line2 (- y1) x1 (- y2) x2)
                    midpoint)))

(defn displace-at-bisector [p q d]
  (let [line (gl/line2 p q)
        bisector (scaled-bisector line d)
        curve (geom/random-point bisector)]
    (geom/sample-uniform (bezier/auto-spline2 [p curve q]) 15 true)))

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn scene []
  (let [d1 0.25
        d2 0.25
        line (gl/line2 (r 0.1 0.66) (r 0.9 0.66))
        bisector (scaled-bisector line d2)]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 0.2}
              (concat (for [i (range 20)]
                        (svg/polyline (displace-line (r 0.1 0.33) (r 0.9 0.33) d1)
                                      {:key (str "line" i)}))
                      [(with-meta line {:stroke "blue" :key "a"})
                       (with-meta bisector {:stroke "blue" :key "b"})]
                      (for [i (range 20)]
                        (svg/polyline (displace-at-bisector (r 0.1 0.66) (r 0.9 0.66) d2)
                                      {:key (str "bisector" i)}))))))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210429
  (ctrl/mount page "svg-host"))

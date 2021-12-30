(ns shimmers.sketches.path-distribution
  "Perturbations of displaced lines."
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn displace-line
  "Generate a set of points on a curved line between p and q by picking a confused
  midpoint."
  [line d]
  (let [[p q] (g/vertices line)
        curve (geometry/confused-midpoint p q d)]
    (g/sample-uniform (bezier/auto-spline2 [p curve q]) 15 true)))

;; https://stackoverflow.com/a/45701864/34450
(defn scaled-bisector [line d]
  (let [[p q] (g/vertices line)
        midpoint (tm/mix p q 0.5)
        [x1 y1] (tm/* (tm/- p midpoint) d)
        [x2 y2] (tm/* (tm/- q midpoint) d)]
    (g/translate (gl/line2 (- y1) x1 (- y2) x2)
                    midpoint)))

(defn displace-at-bisector [line d]
  (let [[p q] (g/vertices line)
        bisector (scaled-bisector line d)
        curve (g/random-point bisector)]
    (g/sample-uniform (bezier/auto-spline2 [p curve q]) 15 true)))

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn scene [d]
  (let [line1 (gl/line2 (r 0.1 0.25) (r 0.9 0.25))
        line2 (gl/line2 (r 0.1 0.75) (r 0.9 0.75))
        bisector2 (scaled-bisector line2 d)]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 0.2}
              (concat [(with-meta line1 {:stroke "blue"})
                       (with-meta (gc/circle (g/point-at line1 0.5)
                                             (* d 0.5 (apply g/dist (g/vertices line1))))
                         {:stroke "blue" :fill "none"})]
                      (for [_ (range 20)]
                        (svg/polyline (displace-line line1 d)))
                      [(with-meta line2 {:stroke "blue"})
                       (with-meta bisector2 {:stroke "blue"})]
                      (for [_ (range 20)]
                        (svg/polyline (displace-at-bisector line2 d)))))))

(defn page []
  (let [d 0.3]
    [:div
     [:div.canvas-frame (scene d)]
     [:h4 "Two approaches for adding random curvature to a line between p and q"]
     [:p "Both are using a displacement factor " d ", multiplied by
     half the distance from " [:b "p"]  " to " [:b "q"]
      ". The upper example is drawing a curved line through a random point in
      the circle. The lower example picks a random point on the bisector."]]))

(sketch/definition path-distribution
  {:created-at "2021-04-29"
   :type :svg
   :tags #{:demo}}
  (ctrl/mount page "sketch-host"))

(ns shimmers.sketches.clustered-farmlands
  (:require [kixi.stats.distribution :as ksd]
            [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]))

(defn randnorm [mu sd]
  (ksd/draw (ksd/normal {:mu mu :sd sd})))

(defn random-offsets-spaced [lower upper spacing]
  (for [o (range lower upper spacing)]
    (+ o (* spacing (randnorm 0 0.1)))))

(def width 800)
(def height 800)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

;; TODO curve the "road" and add houses + lots along it, spaced at some interval
;; curve the field rows with dampened displacement along the path?
;; Break up the field rows occasionally and sometimes match the rows above and below
;; Add varying green levels per row
(defn scene []
  (let [spacing 0.05
        road (bezier/auto-spline2 [(r (randnorm 0.5 0.1) (- (* 2 spacing)))
                                   (r (randnorm 0.5 0.01) 0.3)
                                   (r (randnorm 0.5 0.01) 0.7)
                                   (r (randnorm 0.5 0.1) (+ 1 (* 2 spacing)))])
        ;; Trying to make them line up, but to be fields I think they have to be
        ;; separate to fill later
        rows (for [y (random-offsets-spaced 0 1 spacing)
                   :let [mid (geom/point-at road y)]
                   :when mid]
               (bezier/auto-spline2 [(r 0 y) mid (r 1 y)]))]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 0.5}
              (svg/polyline (geom/sample-uniform road 10 true)
                            {:stroke-width 5})
              (for [row rows]
                (svg/polyline (geom/sample-uniform row 10 true))))))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210510
  (ctrl/mount page "svg-host"))

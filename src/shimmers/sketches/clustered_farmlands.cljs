(ns shimmers.sketches.clustered-farmlands
  (:require [kixi.stats.distribution :as ksd]
            [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]))

(defn random-offsets-spaced [lower upper spacing]
  (for [o (range lower upper spacing)]
    (+ o (* spacing (ksd/draw (ksd/normal {:mu 0 :sd 0.1}))))))

(def width 800)
(def height 800)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

;; TODO curve the "road" and add houses + lots along it, spaced at some interval
;; curve the field rows with dampened displacement along the path?
;; Break up the field rows occasionally and sometimes match the rows above and below
;; Add varying green levels per row
(defn scene []
  (let [road (gl/line2 (r 0.5 0.0) (r 0.5 1.0))]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 0.2}
              (with-meta road {:stroke-width 10})
              (for [y (random-offsets-spaced 0 1 0.05)]
                (gl/line2 (r 0.5 y) (r 0 y)))
              (for [y (range 0 1 0.05)]
                (gl/line2 (r 0.5 y) (r 1.0 y))))))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210510
  (ctrl/mount page "svg-host"))

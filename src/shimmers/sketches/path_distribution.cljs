(ns shimmers.sketches.path-distribution
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.svg.core :as svg]))

(defn scene []
  (csvg/svg {:width 800 :height 600}
            (svg/path [[:M [50 50]] [:L [250 50]]] {:stroke "black"})))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210429
  (ctrl/mount page "svg-host"))

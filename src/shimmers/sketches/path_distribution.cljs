(ns shimmers.sketches.path-distribution
  (:require [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]))

(defn svg
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs & body]
  (into [:svg
         (svg/svg-attribs
          attribs
          {:xmlns "http://www.w3.org/2000/svg"})]
        body))

(defn scene []
  (svg {:width 800 :height 600}
       (svg/path [[:M [50 50]] [:L [250 50]]] {:stroke "black"})))

(defn page []
  [:div (adapt/all-as-svg (scene))])

(defn ^:export run-sketch []
  ;; 20210429
  (ctrl/mount page "svg-host"))

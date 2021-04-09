(ns shimmers.sketches.uniform-distribution
  (:require [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.svg.adapter :as adapt]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]))

(defn svg
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs & body]
  (into [:svg
         (svg/svg-attribs
          attribs
          {:xmlns "http://www.w3.org/2000/svg"})]
        body))

(defn scene []
  (let [c1 (gc/circle 150 150 100)
        points (for [i (range 1000)]
                 (let [[x y] (geom/random-point-inside c1)]
                   (with-meta (gc/circle x y 1) {:key (str "c-" i)})))]
    (svg {:width 900 :height 600}
         (svg/group {:fill "none" :stroke "black"} c1)
         (svg/group {:fill "black" :opacity 0.6} points))))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

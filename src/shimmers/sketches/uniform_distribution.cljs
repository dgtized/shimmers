(ns shimmers.sketches.uniform-distribution
  (:require [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]))

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
        points1
        (for [i (range 1000)]
          (let [[x y] (geom/random-point-inside c1)]
            (with-meta (gc/circle x y 1) {:key (str "c1-" i)})))
        c2 (gc/circle 500 150 100)
        points2
        (for [i (range 1000)]
          (let [[x y] (p/confusion-disk [500 150] 100)]
            (with-meta (gc/circle x y 1) {:key (str "c2-" i)})))]
    (svg {:width 900 :height 600 :stroke "black"}
         (svg/text (gv/vec2 150 30) "g/random-point-inside circle" {:text-anchor "middle"})
         (svg/group {:fill "none"} c1)
         (svg/group {:fill "black" :opacity 0.6} points1)
         (svg/text (gv/vec2 500 30) "g/sample-uniform-inside circle" {:text-anchor "middle"})
         (svg/group {:fill "none"} c2)
         (svg/group {:fill "black" :opacity 0.6} points2))))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

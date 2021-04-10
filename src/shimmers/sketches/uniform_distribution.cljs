(ns shimmers.sketches.uniform-distribution
  (:require [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn svg
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs & body]
  (into [:svg
         (svg/svg-attribs
          attribs
          {:xmlns "http://www.w3.org/2000/svg"})]
        body))

(defn example [pos shape sample-method description]
  (svg/group {}
             (svg/text (tm/- pos (gv/vec2 0 120))
                       description
                       {:text-anchor "middle"})
             (svg/group {:fill "none"} shape)
             (svg/group {:fill "black" :opacity 0.6}
                        (for [i (range 500)
                              :let [[x y] (sample-method shape)]]
                          (with-meta (gc/circle x y 1) {:key (str description "-" i)})))))

(defn scene []
  (svg {:width 900 :height 600 :stroke "black"}
       (example (gv/vec2 150 150) (gc/circle 150 150 100)
                geom/random-point-inside
                "g/random-point-inside circle")
       (example (gv/vec2 450 150) (gc/circle 450 150 100)
                (fn [_] (p/confusion-disk (gv/vec2 450 150) 100))
                "g/sample-uniform-inside circle")
       (example (gv/vec2 150 400) (rect/rect 100 300 100 100)
                geom/random-point-inside
                "g/random-point-inside rect")))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

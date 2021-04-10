(ns shimmers.sketches.uniform-distribution
  (:require [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.triangle :as gt]
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
             (svg/text (tm/+ pos (gv/vec2 0 -70))
                       description
                       {:text-anchor "middle"})
             (svg/group {:fill "none"} (geom/translate shape pos))
             (svg/group {:fill "black" :opacity 0.6}
                        (for [i (range 500)
                              :let [[x y] (sample-method shape)]]
                          (with-meta (geom/translate (gc/circle x y 1) pos)
                            {:key (str description "-" i)})))))

(defn scene []
  (svg {:width 900 :height 600 :stroke "black"}
       (example (gv/vec2 100 100) (gc/circle 0 0 50)
                geom/random-point-inside
                "g/random-point-inside circle")
       (example (gv/vec2 350 100) (gc/circle 0 0 50)
                (fn [_] (p/confusion-disk (gv/vec2 0 0) 50))
                "g/sample-uniform-inside circle")
       (example (gv/vec2 100 250) (geom/center (rect/rect 0 0 100 100))
                geom/random-point-inside
                "g/random-point-inside rect")
       (example (gv/vec2 100 400) (geom/center (gt/triangle2 (gv/vec2 0 0) (gv/vec2 100 50) (gv/vec2 25 100)))
                geom/random-point-inside
                "g/random-point-inside triangle")
       (example (gv/vec2 350 400) (geom/center (gt/triangle2 (gv/vec2 0 0) (gv/vec2 100 50) (gv/vec2 25 100)))
                geometry/random-point-in-triangle2
                "g/sample-uniform-inside triangle")))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

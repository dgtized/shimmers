(ns shimmers.sketches.uniform-distribution
  (:require [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
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
  (let [shape-centered (geom/center shape)]
    (svg/group {}
               (svg/text (tm/+ pos (gv/vec2 0 -70))
                         description
                         {:text-anchor "middle"})
               (svg/group {:fill "none" :stroke "red"} (geom/translate shape-centered pos))
               (svg/group {:opacity 0.8}
                          (for [i (range 500)
                                :let [[x y] (sample-method shape-centered)]]
                            (with-meta (geom/translate (gc/circle x y 0.5) pos)
                              {:key (str description "-" i)}))))))

(defn scene []
  (let [circle (gc/circle 0 0 50)
        ;; ellipse is not implemented?
        rectangle (rect/rect 0 0 100 100)
        rotated-rectangle (geom/rotate rectangle 0.25)
        triangle (gt/triangle2 (gv/vec2 0 0) (gv/vec2 100 50) (gv/vec2 25 100))
        polygon (gp/polygon2 [0 0] [50 75] [100 100] [100 50] [75 25])
        ]
    (svg {:width 900 :height 600 :stroke "black"}
         (example (gv/vec2 100 100) circle
                  geom/random-point-inside
                  "g/random-point-inside circle")
         (example (gv/vec2 300 100) circle
                  (fn [_] (p/confusion-disk (gv/vec2 0 0) 50))
                  "random point uniform circle")
         (example (gv/vec2 500 100) circle
                  geom/random-point
                  "g/random-point circle")
         #_(example (gv/vec2 700 100) circle
                    (fn [s] (geom/sample-uniform s 100 true))
                    "g/sample-uniform circle")

         (example (gv/vec2 100 250) rectangle
                  geom/random-point-inside
                  "g/random-point-inside rect")
         (example (gv/vec2 300 250) rotated-rectangle
                  geom/random-point-inside
                  "g/random-point-inside rect")
         (example (gv/vec2 500 250) rectangle
                  geom/random-point
                  "g/random-point rect")
         #_(example (gv/vec2 700 250) rectangle
                    (fn [s] (geom/sample-uniform s 0.1 true))
                    "g/sample-uniform rect")

         (example (gv/vec2 100 400) triangle
                  geom/random-point-inside
                  "g/random-point-inside triangle")
         (example (gv/vec2 300 400) triangle
                  geometry/random-point-in-triangle2
                  "random point uniform triangle")
         (example (gv/vec2 500 400) triangle
                  geom/random-point
                  "g/random-point triangle")
         (example (gv/vec2 300 550) polygon
                  geom/random-point-inside
                  "g/random-point-inside polygon")
         (example (gv/vec2 500 550) polygon
                  geom/random-point
                  "g/random-point polygon"))))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

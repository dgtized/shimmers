(ns shimmers.sketches.uniform-distribution
  (:require [shimmers.common.svg :refer [svg]]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn sample-points
  ([shape sample-method] (sample-points shape sample-method 400))
  ([shape sample-method n]
   (let [centered (g/center shape)]
     (repeatedly n #(sample-method centered)))))

(defn example [pos shape points description]
  (let [shape-centered (g/center shape)]
    (svg/group {}
               (svg/text (tm/+ pos (gv/vec2 0 -70))
                         description
                         {:text-anchor "middle"})
               (svg/group {:fill "none" :stroke "red"} (g/translate shape-centered pos))
               (svg/group {:opacity 0.8}
                          (for [[i [x y]] (map-indexed vector points)]
                            (with-meta (g/translate (gc/circle x y 0.5) pos)
                              {:key (str description "-" i)}))))))

(defn scene []
  (let [circle (gc/circle 0 0 50)
        ;; ellipse is not implemented?
        rectangle (rect/rect 0 0 100 100)
        rotated-rectangle (g/rotate rectangle 0.25)
        triangle (gt/triangle2 (gv/vec2 0 0) (gv/vec2 100 50) (gv/vec2 25 100))
        polygon (gp/polygon2 [0 0] [50 75] [100 100] [100 50] [75 25])]
    (svg {:width 800 :height 600 :stroke "black"}
         (example (gv/vec2 100 100) circle
                  (sample-points circle g/random-point-inside)
                  "g/random-point-inside")
         (example (gv/vec2 300 100) circle
                  (sample-points circle geometry/random-point-in-circle)
                  "random-point-in-circle")
         (example (gv/vec2 500 100) circle
                  (sample-points circle g/random-point 200)
                  "g/random-point")
         (example (gv/vec2 700 100) circle
                  (g/sample-uniform (g/center circle) 10 true)
                  "g/sample-uniform")

         (example (gv/vec2 100 250) rectangle
                  (sample-points rectangle g/random-point-inside)
                  "g/random-point-inside")
         (example (gv/vec2 300 250) rotated-rectangle
                  (sample-points rotated-rectangle g/random-point-inside)
                  "g/random-point-inside (polygon)")
         (example (gv/vec2 500 250) rectangle
                  (sample-points rectangle g/random-point 200)
                  "g/random-point")
         (example (gv/vec2 700 250) rectangle
                  (g/sample-uniform (g/center rectangle) 10 true)
                  "g/sample-uniform")

         (example (gv/vec2 100 400) triangle
                  (sample-points triangle g/random-point-inside)
                  "g/random-point-inside")
         (example (gv/vec2 300 400) triangle
                  (sample-points triangle geometry/random-point-in-triangle)
                  "random-point-in-triangle")
         (example (gv/vec2 500 400) triangle
                  (sample-points triangle g/random-point 200)
                  "g/random-point")
         (example (gv/vec2 700 400) triangle
                  (g/sample-uniform (g/center triangle) 10 true)
                  "g/sample-uniform")

         (example (gv/vec2 300 550) polygon
                  (sample-points polygon g/random-point-inside)
                  "g/random-point-inside")
         (example (gv/vec2 500 550) polygon
                  (sample-points polygon g/random-point 200)
                  "g/random-point")
         (example (gv/vec2 700 550) polygon
                  (g/sample-uniform (g/center polygon) 10 true)
                  "g/sample-uniform"))))

(sketch/definition uniform-distribution
  {:created-at "2021-04-09"
   :type :svg
   :tags #{:static :demo}}
  (ctrl/mount scene "canvas-host"))

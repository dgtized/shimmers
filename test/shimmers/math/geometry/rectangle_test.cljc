(ns shimmers.math.geometry.rectangle-test
  (:require [shimmers.math.geometry.rectangle :as sut]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]))


(deftest trim-axis-aligned-ears
  (is
   (= [(rect/rect 0 4 4 6)
       (rect/rect 4 0 6 10)]
      (sut/trim-axis-aligned-ears
       (gp/polygon2 (gv/vec2 0 4) (gv/vec2 4 4) (gv/vec2 4 0)
                    (gv/vec2 10 0) (gv/vec2 10 10) (gv/vec2 0 10))))
   "L-shape upper left corner is cut out")
  (is
   (= [(rect/rect 0 0 4 10)
       (rect/rect 4 0 6 6)]
      (sut/trim-axis-aligned-ears
       (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 6)
                    (gv/vec2 4 6) (gv/vec2 4 10) (gv/vec2 0 10))))
   "L-shape lower right corner is missing")

  (is
   (= [(rect/rect 0 0 7 4)
       (rect/rect 3 4 4 6)
       (rect/rect 7 0 3 4)]
      (sut/trim-axis-aligned-ears
       (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 4)
                    (gv/vec2 7 4) (gv/vec2 7 10) (gv/vec2 3 10)
                    (gv/vec2 3 4) (gv/vec2 0 4))))
   "T-shape with box extruded")

  (is
   (= [(rect/rect 0 0 7 5)
       (rect/rect 3 5 4 5)
       (rect/rect 7 0 3 4)]
      (sut/trim-axis-aligned-ears
       (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 4)
                    (gv/vec2 7 4) (gv/vec2 7 10) (gv/vec2 3 10)
                    (gv/vec2 3 5) (gv/vec2 0 5))))
   "T-shape with uneven base for extruded box")

  (is
   (= [(rect/rect 0 0 3 10)
       (rect/rect 3 0 3 6)
       (rect/rect 6 0 4 2)]
      (sut/trim-axis-aligned-ears
       (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 2)
                    (gv/vec2 6 2) (gv/vec2 6 6) (gv/vec2 3 6)
                    (gv/vec2 3 10) (gv/vec2 0 10))))
   "ears on ears")

  (is
   (= [(rect/rect 0 0 6 10)
       (rect/rect 6 7 2 3)
       (rect/rect 6 0 4 3)]
      (sut/trim-axis-aligned-ears
       (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 3)
                    (gv/vec2 6 3) (gv/vec2 6 7) (gv/vec2 8 7)
                    (gv/vec2 8 10) (gv/vec2 0 10))))
   "C-shape with mismatched extensions")

  ;; TODO rotations, opposite corners, identity and expected output?
  )

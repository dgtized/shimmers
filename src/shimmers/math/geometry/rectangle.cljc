(ns shimmers.math.geometry.rectangle
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Rect2]])
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm])
  #?(:clj
     (:import [thi.ng.geom.types Rect2])))

(defn left-side [rect]
  (gl/line2 (rect/bottom-left rect)
            (gv/vec2 (rect/left rect) (rect/top rect))))

(defn right-side [rect]
  (gl/line2 (gv/vec2 (rect/right rect) (rect/bottom rect))
            (rect/top-right rect)))

(defn top-side [rect]
  (gl/line2 (gv/vec2 (rect/left rect) (rect/top rect))
            (rect/top-right rect)))

(defn bottom-side [rect]
  (gl/line2 (rect/bottom-left rect)
            (gv/vec2 (rect/right rect) (rect/bottom rect))))

(comment
  (right-side (rect/rect 5))
  (left-side (rect/rect 5))
  (map g/bounds (lines/cut-polygon (rect/rect 10) (left-side (rect/rect 2 0 6 10)))))

(defn polygon->rectangle
  "Convert a `polygon` to a `Rect2` iff it has 4 points and they are axis aligned.

  Otherwise return the polygon as is."
  [polygon]
  (let [vertices (g/vertices polygon)]
    (if (and (= 4 (count vertices))
             ;; check axis-aligned
             ;; FIXME: handle counter clockwise orderings?
             (let [[[ax ay] [bx by] [cx cy] [dx dy]] vertices]
               (or
                ;; a-b or c-d
                ;; d-c    b-a
                (and (tm/delta= ax dx)
                     (tm/delta= bx cx)
                     (tm/delta= ay by)
                     (tm/delta= cy dy))
                ;; b-c or d-a
                ;; a-d    c-b
                (and (tm/delta= ax bx)
                     (tm/delta= cx dx)
                     (tm/delta= by cy)
                     (tm/delta= dy ay)))))
      (g/bounds polygon)
      polygon)))

;; see also https://blog.thebehrens.net/2010/03/03/clipping-boxes/

;; for each edge, check to see if continuing would clip another edge, if so,
;; lines/cut-polygon with that line and recurse on each remaining?
;; might cause extra cuts if T shape with mismatched depths.
(defn trim-axis-aligned-ears [polygon]
  (let [rect (polygon->rectangle polygon)]
    (if (instance? Rect2 rect)
      [rect]
      (let [vertices (g/vertices polygon)
            slice (some (fn [[a b c]]
                          (when (> (v/orient2d a b c) 0)
                            (g/scale-size (gl/line2 b c) 1000)))
                        (partition 3 1 (conj vertices (first vertices))))]
        (if slice
          (mapcat trim-axis-aligned-ears (lines/cut-polygon polygon slice))
          [polygon])))))

(comment
  ;; L-shape upper left corner is cut out
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 4) (gv/vec2 4 4) (gv/vec2 4 0)
                (gv/vec2 10 0) (gv/vec2 10 10) (gv/vec2 0 10)))
  ;; L-shape lower right corner is missing
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 6)
                (gv/vec2 4 6) (gv/vec2 4 10) (gv/vec2 0 10)))

  ;; T-shape with box extruded
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 4)
                (gv/vec2 7 4) (gv/vec2 7 10) (gv/vec2 3 10)
                (gv/vec2 3 4) (gv/vec2 0 4)))

  ;; T-shape with uneven base for extruded box
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 4)
                (gv/vec2 7 4) (gv/vec2 7 10) (gv/vec2 3 10)
                (gv/vec2 3 5) (gv/vec2 0 5)))

  ;; ears on ears
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 2)
                (gv/vec2 6 2) (gv/vec2 6 6) (gv/vec2 3 6)
                (gv/vec2 3 10) (gv/vec2 0 10)))

  ;; C-shape with mismatched extensions
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 3)
                (gv/vec2 6 3) (gv/vec2 6 7) (gv/vec2 8 7)
                (gv/vec2 8 10) (gv/vec2 0 10)))

  ;; TODO rotations, opposite corners, identity and expected output?
  )


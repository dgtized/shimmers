(ns shimmers.math.geometry.rectangle
  (:require
   [shimmers.algorithm.lines :as lines]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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

;; for each edge, check to see if continuing would clip another edge, if so,
;; lines/cut-polygon with that line and recurse on each remaining?
;; might cause extra cuts if T shape with mismatched depths.
(defn trim-axis-aligned-ears [polygon]
  [polygon])

(comment
  ;; upper left corner is cut out
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 4) (gv/vec2 4 4) (gv/vec2 4 0)
                (gv/vec2 10 0) (gv/vec2 10 10) (gv/vec2 0 10)))
  ;; lower right corner is missing
  (trim-axis-aligned-ears
   (gp/polygon2 (gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 10 6)
                (gv/vec2 4 6) (gv/vec2 4 10) (gv/vec2 0 10)))

  ;; TODO: examples with more then one "ear" on opposite sides
  ;; or ears on ears
  )


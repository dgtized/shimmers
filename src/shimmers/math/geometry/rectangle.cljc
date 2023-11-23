(ns shimmers.math.geometry.rectangle
  (:require
   [shimmers.algorithm.lines :as lines]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
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
             (let [[[ax ay] [bx by] [cx cy] [dx dy]] vertices]
               (and (tm/delta= ax dx)
                    (tm/delta= bx cx)
                    (tm/delta= ay by)
                    (tm/delta= cy dy))))
      (g/bounds polygon)
      polygon)))

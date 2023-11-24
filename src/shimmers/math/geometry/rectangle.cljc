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

(defn trim-axis-aligned-ears
  "Cuts polygon with axis-aligned edges into constituent rectangles

  Does not check if polygon is axis-aligned, so will bail if encountering
  diagonal edges or the like."
  [polygon]
  (let [rect (polygon->rectangle polygon)]
    (if (instance? Rect2 rect)
      [rect]
      (let [vertices (g/vertices polygon)
            slice (some (fn [[a b c]]
                          (when (> (v/orient2d a b c) 0)
                            (g/scale-size (gl/line2 b c) 1000)))
                        (partition 3 1 (conj vertices (first vertices))))]
        (if slice
          (let [cuts (lines/cut-polygon polygon slice)]
            (if (> (count cuts) 1)
              (mapcat trim-axis-aligned-ears cuts)
              ;; cut did not split polygon, so bail early
              cuts))
          [polygon])))))


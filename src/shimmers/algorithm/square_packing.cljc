(ns shimmers.algorithm.square-packing
  (:require [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

;; Row Major
;; 111
;; 203
;; 444

;; Column Major
;; 124
;; 104
;; 134

;; Clockwise
;; 4 1 1
;; 4 0 2
;; 3 3 2

;; TODO: support splits like?
;; 123 112 122
;; 405 302 103
;; 678 344 144

(defn row-major [{[w h] :size}]
  (if (> h w) :row :column))

;; Note that px,py are not clamped to 0,1 so some funky but interesting results
;; are possible if using values outside of the range.
;; TODO: support splitting out arbitrary rectangles and not just squares
(defn split-panes
  "Split a rectangle into a square and the 4 surrounding rectangles. The square is
  of `size`, with `px,py` indicating percent positioning within the larger
  rectangle. row-major indicates if the panes should split by rows and then fill
  in the gaps east and west of the square, or by columns and fill in the gaps
  north or south of the square.

  Depending on the placement and size of the square, some of the surrounding
  rectangles may have length or width zero."
  [{p :p [width height] :size} size [percent-x percent-y] split]
  (let [ix (* percent-x (- width size))
        iy (* percent-y (- height size))
        inner (rect/rect (tm/+ p [ix iy]) size size)]
    (->> (case split
           :row
           [(rect/rect p width iy) ;; south row
            (rect/rect (tm/+ p [0 (+ size iy)]) width (- height size iy)) ;; north row
            (rect/rect (tm/+ p [0 iy]) ix size) ;; east chunk
            (rect/rect (tm/+ p [(+ ix size) iy]) (- width size ix) size) ;; west chunk
            ]
           :column
           [(rect/rect (tm/+ p [ix 0]) size iy) ;; south chunk
            (rect/rect (tm/+ p [ix (+ size iy)]) size (- height size iy)) ;; north chunk
            (rect/rect p ix height) ;; east column
            (rect/rect (tm/+ p [(+ size ix) 0]) (- width size ix) height) ;; west column
            ]
           :clockwise
           [(rect/rect (tm/+ p [ix 0]) (- width ix) iy) ; top
            (rect/rect (tm/+ p [(+ size ix) iy]) (- width size ix) (- height iy)) ; right
            (rect/rect (tm/+ p [0 (+ iy size)]) (+ ix size) (- height iy size)) ; bottom
            (rect/rect p ix (+ iy size))]) ; left
         (into [inner]))))

(defn has-area? [{:keys [size]}]
  (every? pos? size))

(defn proportional-split
  ([rectangle ratio percent]
   (proportional-split rectangle ratio percent (row-major rectangle)))
  ([rectangle ratio percent split]
   (let [{[w h] :size} rectangle
         square (* (min w h) ratio)]
     (filter has-area? (split-panes rectangle square percent split)))))

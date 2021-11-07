(ns shimmers.algorithm.square-packing
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
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
;; 3 4 4
;; 3 0 1
;; 2 2 1

;; TODO: support splits like?
;; 123 112 122
;; 405 302 103
;; 678 344 144

(defn row-major [{[w h] :size}]
  (if (> h w) :row :column))

(defn surrounding-panes
  "Given an outer rectangle and an inner rectangle with relative coordinates to
  the outer rectangle, split out the surrounding panes that are remaining from
  the outer rectangle. The panes returned will be in the relative coordinate
  space. "
  [{[width height] :size}
   {[x y] :p [w h] :size}
   split]
  (case split
    :row
    [(rect/rect (gv/vec2) width y) ;; south row
     (rect/rect (gv/vec2 0 (+ h y)) width (- height h y)) ;; north row
     (rect/rect (gv/vec2 0 y) x h) ;; east chunk
     (rect/rect (gv/vec2 (+ x w) y) (- width w x) h) ;; west chunk
     ]
    :column
    [(rect/rect (gv/vec2 x 0) w y) ;; south chunk
     (rect/rect (gv/vec2 x (+ h y)) w (- height h y)) ;; north chunk
     (rect/rect (gv/vec2) x height) ;; east column
     (rect/rect (gv/vec2 (+ w x) 0) (- width w x) height) ;; west column
     ]
    :clockwise
    [(rect/rect (gv/vec2 (+ w x) y) (- width w x) (- height y)) ; right
     (rect/rect (gv/vec2 0 (+ y h)) (+ x w) (- height y h)) ; bottom
     (rect/rect (gv/vec2) x (+ y h)) ; left
     (rect/rect (gv/vec2 x 0) (- width x) y)] ; top
    ))

;; Note that px,py are not clamped to 0,1 so some funky but interesting results
;; are possible if using values outside of the range.
(defn split-panes
  "Split a rectangle into a square and the 4 surrounding rectangles. The square is
  of `size`, with `px,py` indicating percent positioning within the larger
  rectangle. row-major indicates if the panes should split by rows and then fill
  in the gaps east and west of the square, or by columns and fill in the gaps
  north or south of the square.

  Depending on the placement and size of the square, some of the surrounding
  rectangles may have length or width zero."
  [{p :p [width height] :size :as outer} size [percent-x percent-y] split]
  (let [pos (gv/vec2 [(* percent-x (- width size)) (* percent-y (- height size))])
        inner (rect/rect pos size size)]
    (mapv (fn [s] (g/translate s p))
          (into [inner] (surrounding-panes outer inner split)))))

(defn has-area? [{:keys [size]}]
  (every? pos? size))

(defn proportional-split
  ([rectangle ratio percent]
   (proportional-split rectangle ratio percent (row-major rectangle)))
  ([rectangle ratio percent split]
   (let [{[w h] :size} rectangle
         square (* (min w h) ratio)]
     (filter has-area? (split-panes rectangle square percent split)))))

(ns shimmers.algorithm.square-packing
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

;; Row Major & Column Major
;; 111         124
;; 203         104
;; 444         134

;; Clockwise & Counter Clockwise
;; 3 4 4       2 2 1
;; 3 0 1       3 0 1
;; 2 2 1       3 4 4

;; All
;; 1 2 3
;; 4 0 5
;; 6 7 8

;; TODO: support splits like?
;; 122
;; 103
;; 144

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
    :counter-clockwise
    [(rect/rect (gv/vec2 (+ w x) 0) (- width w x) (+ y h))
     (rect/rect (gv/vec2) (+ x w) y)
     (rect/rect (gv/vec2 0 y) x (- height y))
     (rect/rect (gv/vec2 x (+ y h)) (- width x) (- height y h))]
    :all ;; top row
    [(rect/rect (gv/vec2 0 0) x y)
     (rect/rect (gv/vec2 x 0) w y)
     (rect/rect (gv/vec2 (+ x w) 0) (- width w x) y)
     ;; middle row without inner rectangle
     (rect/rect (gv/vec2 0 y) x h)
     (rect/rect (gv/vec2 (+ w x) y) (- width w x) h)
     ;; bottom row
     (rect/rect (gv/vec2 0 (+ y h)) x (- height h y))
     (rect/rect (gv/vec2 x (+ y h)) w (- height h y))
     (rect/rect (gv/vec2 (+ x w) (+ y h)) (- width w x) (- height h y))]))

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

(defn place-by
  "Place a `candidate` shape on a particular side of a `fixed` shape.

  Uses g/bounds of both shapes to ensure they do not overlap."
  [side fixed candidate]
  (let [bounds-fixed (g/bounds fixed)
        bounds-candidate (g/bounds candidate)
        t (case side
            :left
            (gv/vec2 (- (rect/right bounds-fixed) (rect/left bounds-candidate)) 0)
            :right
            (gv/vec2 (- (rect/left bounds-fixed) (rect/right bounds-candidate)) 0))]
    (g/translate candidate t)))

(comment (place-by :left (rect/rect 10) (rect/rect 0 0 20))
         (place-by :left (rect/rect 10) (rect/rect -10 0 20))
         (place-by :left (rect/rect 10) (rect/rect 10 0 20))
         (place-by :right (rect/rect 10) (rect/rect 0 0 20))
         (place-by :right (rect/rect 10) (rect/rect -10 0 20))
         (place-by :right (rect/rect 10) (rect/rect 10 0 20)))

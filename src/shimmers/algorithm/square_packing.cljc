(ns shimmers.algorithm.square-packing
  (:require [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

;; Note that px,py are not clamped to 0,1 so some funky but interesting results
;; are possible if using values outside of the range.
(defn split-panes
  "Split a rectangle into a square and the 4 surrounding rectangles. The square is
  of `size`, with `px,py` indicating percent positioning within the
  larger rectangle. row-major indicates if the panes should split by rows and
  then fill in the gaps east and west of the square, or by columns and fill in
  the gaps north or south of the square."
  ([{[w h] :size :as rectangle} size percent]
   (split-panes rectangle size percent (> h w)))
  ([{p :p [width height] :size} size [px py] row-major]
   (let [offset-x (* px (- width size))
         offset-y (* py (- height size))
         sq (rect/rect (tm/+ p [offset-x offset-y]) size size)]
     (->> (if row-major
            [(rect/rect p width offset-y) ;; south row
             (rect/rect (tm/+ p [0 (+ size offset-y)]) width (- height size offset-y)) ;; north row
             (rect/rect (tm/+ p [0 offset-y]) offset-x size) ;; east chunk
             (rect/rect (tm/+ p [(+ offset-x size) offset-y]) (- width size offset-x) size) ;; west chunk
             ]
            [(rect/rect (tm/+ p [offset-x 0]) size offset-y) ;; south chunk
             (rect/rect (tm/+ p [offset-x (+ size offset-y)]) size (- height size offset-y)) ;; north chunk
             (rect/rect p offset-x height) ;; east column
             (rect/rect (tm/+ p [(+ size offset-x) 0]) (- width size offset-x) height) ;; west column
             ])
          (into [sq])))))

(defn has-area? [{:keys [size]}]
  (every? pos? size))


(ns shimmers.algorithm.line-clipping
  "Cohen-sutherland line clipping from https://sighack.com/post/cohen-sutherland-line-clipping-algorithm."
  (:require #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Line2]])
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv])
  #?(:clj (:import [thi.ng.geom.types Polygon2 Line2 Line3])))

(defn encode-endpoint [[x y] r]
  #{(cond (< x (rect/left r))
          :low-x
          (> x (rect/right r))
          :high-x)
    (cond (< y (rect/bottom r))
          :low-y
          (> y (rect/top r))
          :high-y)})

(defn project-y [[x0 y0] [x1 y1] x]
  ;; TODO handle vertical line
  (+ (* (/ (- y1 y0) (- x1 x0))
        (- x x0))
     y0))

(defn project-x [[x0 y0] [x1 y1] y]
  ;; TODO handle horizontal line
  (+ (* (/ (- x1 x0) (- y1 y0))
        (- y y0))
     x0))

(defn clip-point [code rect p q]
  (cond (contains? code :low-x)
        (let [xmin (rect/left rect)]
          (gv/vec2 xmin (project-y p q xmin)))
        (contains? code :high-x)
        (let [xmax (rect/right rect)]
          (gv/vec2 xmax (project-y p q xmax)))
        (contains? code :low-y)
        (let [ymin (rect/bottom rect)]
          (gv/vec2 (project-x p q ymin) ymin))
        (contains? code :high-y)
        (let [ymax (rect/top rect)]
          (gv/vec2 (project-x p q ymax) ymax))))

(defn clip-line [rect p q]
  (let [encode-p (encode-endpoint p rect)
        encode-q (encode-endpoint q rect)]
    (cond (and (empty? encode-p) (empty? encode-q)) ;; both inside rect
          (gl/line2 p q)
          (= encode-p encode-q) ;; both points outside of rect
          nil
          (not-empty encode-p)
          (recur rect (clip-point encode-p rect p q) q)
          :else
          (recur rect p (clip-point encode-q rect p q)))))

(defprotocol IClipped
  (clipped-by [line rect]))

(extend-type Line2
  IClipped
  (clipped-by [{[p q] :points} rect]
    (clip-line rect p q)))

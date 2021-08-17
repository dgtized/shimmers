(ns shimmers.algorithm.line-clipping
  "Cohen-sutherland line clipping from https://sighack.com/post/cohen-sutherland-line-clipping-algorithm."
  (:require [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            #?(:clj [thi.ng.geom.types]
               :cljs [thi.ng.geom.types :refer [Line2]])
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [clojure.set :as set])
  #?(:clj (:import [thi.ng.geom.types Line2])))

(defn encode-endpoint [[x y] r]
  (->> [(cond (< x (rect/left r))
              :low-x
              (> x (rect/right r))
              :high-x)
        (cond (< y (rect/bottom r))
              :low-y
              (> y (rect/top r))
              :high-y)]
       (remove nil?)
       set))

;; not sure how horizontal/vertical lines are not triggering divide by zero, but
;; appears to be working in tests.
(defn project-y [[x0 y0] [x1 y1] x]
  (+ (* (/ (- y1 y0) (- x1 x0))
        (- x x0))
     y0))

(defn project-x [[x0 y0] [x1 y1] y]
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
          (not-empty (set/intersection encode-p encode-q)) ;; both points outside of rect
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

;; adapted from draw-square in
;; https://sighack.com/post/cohen-sutherland-line-clipping-algorithm
(defn hatch-rectangle [rect spacing theta]
  (let [{[x y] :p [w h] :size} rect
        xstart (+ x (tm/random 0 w))
        ystart (+ y (tm/random 0 h))
        cosa (Math/cos theta)
        m (Math/tan theta)
        c (- ystart (* m xstart))

        x0 (- x (/ w 2))
        y0 (+ (* m x0) c)
        x1 (+ x w (/ w 2))
        y1 (+ (* m x1) c)]
    (loop [i 1 hatches [(clip-line rect (gv/vec2 x0 y0) (gv/vec2 x1 y1))]]
      (let [step-term (/ (* i spacing) cosa)
            up (clip-line rect
                          (gv/vec2 x0 (+ y0 step-term))
                          (gv/vec2 x1 (+ y1 step-term)))
            down (clip-line rect
                            (gv/vec2 x0 (- y0 step-term))
                            (gv/vec2 x1 (- y1 step-term)))
            lines (remove nil? [up down])]
        (if (empty? lines)
          hatches
          (recur (inc i) (into hatches lines)))))))

(comment (hatch-rectangle (rect/rect 2 2 2) 0.1 0.1))

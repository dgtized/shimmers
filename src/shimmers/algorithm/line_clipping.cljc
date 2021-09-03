(ns shimmers.algorithm.line-clipping
  "Cohen-sutherland line clipping from https://sighack.com/post/cohen-sutherland-line-clipping-algorithm.

  See also thi.ng.geom.core/clip-with, ie thi.ng.geom.polygon/clip-convex* for
  polygon clipping."
  (:require [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
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

;; Kinda gross but seems to handle floating point errors flip/flopping the
;; bounds for solving for [xmax,y(xmax)] or [x(ymax),ymax]. Without this fix,
;; clip-line can often loop indefinitely. Might be better with a dynamic tolerance?
(def ^:dynamic *tolerance* 0.0001)

(defn clip-point [code rect p q]
  (cond (contains? code :low-x)
        (let [xmin (rect/left rect)]
          (gv/vec2 xmin (project-y p q xmin)))
        (contains? code :high-x)
        (let [xmax (- (rect/right rect) *tolerance*)]
          (gv/vec2 xmax (project-y p q xmax)))
        (contains? code :low-y)
        (let [ymin (rect/bottom rect)]
          (gv/vec2 (project-x p q ymin) ymin))
        (contains? code :high-y)
        (let [ymax (- (rect/top rect) *tolerance*)]
          (gv/vec2 (project-x p q ymax) ymax))))

(defn clip-line [rect init-p init-q]
  (loop [i 0 p init-p q init-q]
    (let [encode-p (encode-endpoint p rect)
          encode-q (encode-endpoint q rect)]
      (cond (and (empty? encode-p) (empty? encode-q)) ;; both inside rect
            (gl/line2 p q)
            (not-empty (set/intersection encode-p encode-q)) ;; both points outside of rect
            nil
            ;; after 4 iterations p and q should have converged to the bounds,
            ;; so treat infinite recursion as a no-solution
            (> i 4)
            nil
            (not-empty encode-p)
            (recur (inc i) (clip-point encode-p rect p q) q)
            :else
            (recur (inc i) p (clip-point encode-q rect p q))))))

(defn hatching-middle-out
  "Add hatching lines above and below the base-line to fill the shape.

  `clip-fn` should return nil if the segment is outside of the shape."
  [clip-fn spacing cosa [x0 y0] [x1 y1]]
  (let [base-line (clip-fn (gv/vec2 x0 y0) (gv/vec2 x1 y1))]
    (loop [i 1 hatches (if base-line [base-line] [])]
      (let [step-term (/ (* i spacing) cosa)
            up (clip-fn (gv/vec2 x0 (+ y0 step-term))
                        (gv/vec2 x1 (+ y1 step-term)))
            down (clip-fn (gv/vec2 x0 (- y0 step-term))
                          (gv/vec2 x1 (- y1 step-term)))
            lines (remove nil? [up down])]
        (if (empty? lines)
          hatches
          (recur (inc i) (into hatches lines)))))))

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
    (hatching-middle-out (partial clip-line rect)
                         spacing cosa
                         [x0 y0] [x1 y1])))

(comment (hatch-rectangle (rect/rect 2 2 2) 0.1 0.1)
         (hatch-rectangle (rect/rect 2 2 2) 0.1 (/ Math/PI 2)))

(defn sqr [x] (* x x))

;; https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
;; Note this is probably broken if segment is entirely inside circle
(defn clip-circle
  "Clip segment `a` to `b` to fit boundary of circle.

  Currently if `a` or `b` lie within the circle, the line returned will extend
  to the boundary of the circle. For hatching this is fine, but it may not be
  expected behavior."
  [{c :p radius :r} a b]
  (let [length-ab (geom/dist a b)
        Dx (/ (- (:x b) (:x a)) length-ab)
        Dy (/ (- (:y b) (:y a)) length-ab)
        t (+ (* Dx (- (:x c) (:x a))) (* Dy (- (:y c) (:y a))))
        E (gv/vec2 (+ (* t Dx) (:x a))
                   (+ (* t Dy) (:y a)))
        length-ec (geom/dist E c)]
    (cond (< length-ec radius) ;; intersects
          (let [dt (Math/sqrt (- (sqr radius) (sqr length-ec)))]
            (gl/line2 (gv/vec2 (+ (:x a) (* Dx (- t dt)))
                               (+ (:y a) (* Dy (- t dt))))
                      (gv/vec2 (+ (:x a) (* Dx (+ t dt)))
                               (+ (:y a) (* Dy (+ t dt))))))
          (tm/delta= length-ec radius) ;; tangent
          nil
          :else ;; doesn't touch (outside or inside?)
          nil)))

(defn hatch-circle [circle spacing theta]
  (let [{[cx _] :p radius :r} circle
        [xstart ystart] (geometry/random-point-in-circle circle)
        cosa (Math/cos theta)
        m (Math/tan theta)
        c (- ystart (* m xstart))

        x0 (- cx (* 1.2 radius))
        y0 (+ (* m x0) c)
        x1 (+ cx (* 1.2 radius))
        y1 (+ (* m x1) c)]
    (hatching-middle-out (partial clip-circle circle)
                         spacing cosa
                         [x0 y0] [x1 y1])))

(ns shimmers.algorithm.line-clipping
  "Cohen-sutherland line clipping from https://sighack.com/post/cohen-sutherland-line-clipping-algorithm.

  See also thi.ng.geom.core/clip-with, ie thi.ng.geom.polygon/clip-convex* for
  polygon clipping."
  (:require
   [clojure.set :as set]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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
(defn hatch-rectangle
  ([rect spacing theta]
   (hatch-rectangle rect spacing theta [(tm/random) (tm/random)]))
  ([rect spacing theta [rx ry]]
   (let [{[x y] :p [w h] :size} rect
         xstart (+ x (* rx w))
         ystart (+ y (* ry h))
         cosa (Math/cos theta)
         m (Math/tan theta)
         c (- ystart (* m xstart))

         x0 (- x (/ w 2))
         y0 (+ (* m x0) c)
         x1 (+ x w (/ w 2))
         y1 (+ (* m x1) c)]
     (hatching-middle-out (partial clip-line rect)
                          spacing cosa
                          [x0 y0] [x1 y1]))))

(comment (hatch-rectangle (rect/rect 2 2 2) 0.1 0.1)
         (hatch-rectangle (rect/rect 2 2 2) 0.1 (/ Math/PI 2)))

;; https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
;; Note this is probably broken if segment is entirely inside circle
(defn clip-circle
  "Clip segment `a` to `b` to fit boundary of circle.

  Currently if `a` or `b` lie within the circle, the line returned will extend
  to the boundary of the circle. For hatching this is fine, but it may not be
  expected behavior."
  [{c :p radius :r} a b]
  (let [length-ab (g/dist a b)
        Dx (/ (- (:x b) (:x a)) length-ab)
        Dy (/ (- (:y b) (:y a)) length-ab)
        t (+ (* Dx (- (:x c) (:x a))) (* Dy (- (:y c) (:y a))))
        E (gv/vec2 (+ (* t Dx) (:x a))
                   (+ (* t Dy) (:y a)))
        length-ec (g/dist E c)]
    (cond (< length-ec radius) ;; intersects
          (let [dt (Math/sqrt (- (eq/sqr radius) (eq/sqr length-ec)))]
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
        [xstart ystart] (rp/inside-circle circle)
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

;; TODO: change x0 a percent of width + height of shape to find start of slashes
(defn variable-hatching [bounds angle x0 n spacing width]
  (let [{[bx by] :p [bw bh] :size} bounds
        m (Math/tan angle)
        cosa (Math/cos angle)
        c (- (+ by bh) (* m x0))
        x0 (- bx (/ bw 2))
        y0 (+ (* m x0) c)
        x1 (+ bx bw (/ bw 2))
        y1 (+ (* m x1) c)]
    (loop [i 0 step 0 slashes []]
      (if (< i n)
        (let [p (gv/vec2 x0 (- y0 step))
              q (gv/vec2 x1 (- y1 step))
              s (spacing)
              w (width)]
          (if-let [line (clip-line bounds p q)]
            (recur (inc i) (+ step (/ (+ s (/ w 2)) cosa))
                   (conj slashes (assoc line :width w)))
            slashes))
        slashes))))

;; Algorithm for arbitrary polygons?
;; http://alienryderflex.com/polygon_hatchline_fill/
;;
;; Not sure if same algorithm, but might work to just find bounds, hatch that
;; bounds, and then do a ray-intersection check and clip using each line?

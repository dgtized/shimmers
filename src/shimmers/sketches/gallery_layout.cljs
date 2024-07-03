(ns shimmers.sketches.gallery-layout
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; 800x600 box with a 3:2 ratio max frame
;; 800/1.5 ~= 800x533
;; 600/1.5 ~= 600x400

(defn frame [{[width height] :size :as rect} [w h]]
  (let [ratio (/ w h)
        box (if (< (/ width ratio) height)
              (rect/rect 0 0 (gv/vec2 width (/ width ratio)))
              (rect/rect 0 0 (gv/vec2 (* height ratio) height)))]
    (vary-meta (g/center box (g/centroid rect))
               assoc :stroke (if (< (/ width ratio) height) "green" "red"))))

(defn display-frame [box frame-ratio]
  (let [picture (frame box frame-ratio)]
    (csvg/group {}
      (vary-meta box assoc :stroke "blue")
      (with-meta (g/scale-size picture 0.9) (meta picture)))))

(defn frame-ratio []
  (dr/weighted {[5 7] 1
                [7 5] 1
                [3 2] 1
                [4 3] 1
                [2 1] 1
                [1 2] 1
                [2 3] 1
                [1 1] 1}))

(defn choose-layout [n]
  (dr/weighted {:row 1
                :column 1
                :grid (if (> n 2) 1 0)
                :diagonal 1}))

(defn wall-layout [wall layout n]
  (case layout
    :diagonal
    (let [angle (dr/random-tau)
          start (g/unmap-point wall (gv/vec2 0.5 0.5))
          len (max (g/width wall) (g/height wall))
          dir (v/polar len angle)
          line (first (lines/clip-line (gl/line2 (tm/- start dir) (tm/+ start dir)) wall))
          line-box (apply rect/rect (g/vertices line))
          rect (if (< (g/width line-box) (g/height line-box))
                 (rect/rect 0 0
                            (min (g/width line-box) (/ (g/width wall) n))
                            (/ (g/height line-box) (inc n)))
                 (rect/rect 0 0
                            (/ (g/width line-box) (inc n))
                            (min (g/height line-box) (/ (g/height wall) n))))]
      (for [t (cs/midsection (tm/norm-range (inc n)))]
        (let [rect (g/center rect (g/point-at line t))]
          (display-frame rect (frame-ratio)))))
    :grid
    (let [area (rect/rect (g/unmap-point wall (gv/vec2 0.0 0.2))
                          (g/unmap-point wall (gv/vec2 1.0 0.8)))]
      (for [box (take n (dr/shuffle (g/subdivide area {:cols (int (/ n 2)) :rows 2})))]
        (display-frame box (frame-ratio))))
    :row
    (let [row (rect/rect (g/unmap-point wall (gv/vec2 0.0 0.33))
                         (g/unmap-point wall (gv/vec2 1.0 0.66)))]
      (for [box (g/subdivide row {:cols n :rows 1})]
        (display-frame box (frame-ratio))))
    :column
    (let [row (rect/rect (g/unmap-point wall (gv/vec2 0.33 0.0))
                         (g/unmap-point wall (gv/vec2 0.66 1.0)))]
      (for [box (g/subdivide row {:cols 1 :rows n})]
        (display-frame box (frame-ratio))))))

;; TODO: instead of randomly generating frame-ratio, should give a pregenerated
;; list of frame ratios to optimize for.
;; TODO: handle 2x1+1 and 3x1+1 cases or 3x with random circular offsets
;; TODO: handle vertical or horizontal displacements to break up varying sizes
(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (let [n (dr/random-int 1 6)]
      (wall-layout
       (csvg/screen width height)
       (choose-layout n)
       n))))

(defn page []
  [sketch/with-explanation
   [:div.canvas-frame [scene]]
   [:div.center [view-sketch/generate :gallery-layout]]
   [:div.readable-width
    [:p "Automatic placement of multiple \"frames\" with varying aspect ratios on a gallery wall."]]])

(sketch/definition gallery-layout
  {:created-at "2023-10-11"
   :tags #{}
   :type :svg}
  (ctrl/mount page))

(ns shimmers.sketches.gallery-layout
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; 800x600 box with a 3:2 ratio max frame
;; 800/1.5 ~= 800x533
;; 600/1.5 ~= 600x400

(defn frame [{[width height] :size :as rect} w h]
  (let [ratio (/ w h)
        box (if (< (/ width ratio) height)
              (rect/rect 0 0 (gv/vec2 width (/ width ratio)))
              (rect/rect 0 0 (gv/vec2 (* height ratio) height)))]
    (vary-meta (g/center box (g/centroid rect))
               assoc :stroke (if (< (/ width ratio) height) "green" "red"))))

(defn display-frame [box w h]
  (csvg/group {}
    (vary-meta box assoc :stroke "blue")
    (frame box w h)))

(defn wall-layout [wall layout n]
  (case layout
    :row
    (let [row (rect/rect (g/unmap-point wall (gv/vec2 0.0 0.33))
                         (g/unmap-point wall (gv/vec2 1.0 0.66)))]
      (for [box (g/subdivide row {:cols n :rows 1})]
        (display-frame box 3 2)))
    :column
    (let [row (rect/rect (g/unmap-point wall (gv/vec2 0.33 0.0))
                         (g/unmap-point wall (gv/vec2 0.66 1.0)))]
      (for [box (g/subdivide row {:cols 1 :rows n})]
        (display-frame box 3 2)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (wall-layout
     (rect/rect 0 0 width height)
     (dr/weighted {:row 1 :column 1})
     (dr/random-int 1 6))))

(sketch/definition gallery-layout
  {:created-at "2023-10-11"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :gallery-layout)))

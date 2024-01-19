(ns shimmers.sketches.star-mosaic
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [bounds base s]
  (concat (clip/hatch-rectangle
           bounds (* s 0.1)
           base [0.5 0.5])
          (clip/hatch-rectangle
           bounds (* s 0.1)
           (+ base (/ eq/TAU 3)) [0.5 0.5])
          (clip/hatch-rectangle
           bounds (* s 0.1)
           (- base (/ eq/TAU 3)) [0.5 0.5])))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "white"
     :stroke-width 0.5}
    (shapes (rect/rect 0 0 width height)
            0
            (* (/ 2 3) width))))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :star-mosaic]
     [:div.readable-width
      "Genuary 2024 - Day 17 - Islamic Tiling"]]))

(sketch/definition star-mosaic
  {:created-at "2024-01-19"
   :tags #{:genuary2024}
   :type :svg}
  (ctrl/mount page))

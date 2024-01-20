(ns shimmers.sketches.triangle-mosaic
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn triangle [[i j] side]
  (let [hside (* side (/ (Math/sqrt 3) 2))
        i (if (odd? j) (inc i) i)
        x (+ (Math/floor (* 0.5 i))
             (if (odd? j) -0.5 0))]
    (if (even? i)
      (gt/triangle2 (gv/vec2 (* (+ x 0.0) side) (* j hside))
                    (gv/vec2 (* (+ x 1.0) side) (* j hside))
                    (gv/vec2 (* (+ x 0.5) side) (* (inc j) hside)))
      (gt/triangle2 (gv/vec2 (* (inc x) side) (* j hside))
                    (gv/vec2 (* (+ (inc x) 0.5) side) (* (inc j) hside))
                    (gv/vec2 (* (- (inc x) 0.5) side) (* (inc j) hside))))))

(defn shapes [{p :p [width height] :size} side]
  (let [wn (/ width side)
        hside (* side (/ (Math/sqrt 3) 2))
        o (tm/+ p (gv/vec2 (* -0.5 side) 0))]
    (for [i (range (inc (* 2 wn)))
          j (range (/ height hside))
          :let [t (g/translate (triangle [i j] side) o)]]
      (csvg/group {}
        (csvg/center-label (g/centroid t) (str [i j]) {:font-size 10})
        t))))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill (csvg/hsl 0.0 0.5 0.5 0.2)
     :stroke-width 1.0}
    (shapes (g/scale-size (rect/rect 0 0 width height) 0.8)
            80)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :triangle-mosaic]
     [:div.readable-width]]))

(sketch/definition triangle-mosaic
    {:created-at "2024-01-19"
     :tags #{}
     :type :svg}
  (ctrl/mount page))

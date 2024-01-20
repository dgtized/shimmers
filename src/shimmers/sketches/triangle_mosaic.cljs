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

(defn triangle [[i j k] side]
  (let [hside (* side (/ (Math/sqrt 3) 2))
        x (+ i (* 0.5 j))]
    (if (zero? k)
      (gt/triangle2 (gv/vec2 (* (+ x 0.0) side) (* j hside))
                    (gv/vec2 (* (+ x 1.0) side) (* j hside))
                    (gv/vec2 (* (+ x 0.5) side) (* (inc j) hside)))
      (gt/triangle2 (gv/vec2 (* (inc x) side) (* j hside))
                    (gv/vec2 (* (+ (inc x) 0.5) side) (* (inc j) hside))
                    (gv/vec2 (* (- (inc x) 0.5) side) (* (inc j) hside))))))

(defn shapes [{p :p [width height] :size} side]
  (let [wn (/ width side)
        hside (* side (/ (Math/sqrt 3) 2))
        o (tm/+ p (gv/vec2 (* -0.2 width) (* -0.5 hside)))]
    (for [i (range wn)
          j (range (/ height hside))
          :let [tl (g/translate (triangle [i j 0] side) o)
                tr (g/translate (triangle [i j 1] side) o)]]
      (csvg/group {}
        (csvg/center-label (g/centroid tl) (str "L" [i j]) {:font-size 10})
        tl
        (csvg/center-label (g/centroid tr) (str "R" [i j]) {:font-size 10})
        tr))))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill (csvg/hsl 0.0 0.5 0.5 0.2)
     :stroke-width 1.0}
    (shapes (g/scale-size (rect/rect 0 0 width height) 0.66)
            100)))

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

(ns shimmers.sketches.color-mapping
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn color [i j]
  (col/as-css (col/hsla (mod (* i (* 3 j) tm/PHI) 1.0)
                        0.5 0.45 1.0)))

(defn shapes []
  (let [[cols rows] [40 30]
        w (/ width cols)
        h (/ height rows)]
    (for [j (range rows)
          i (range cols)]
      (vary-meta (g/scale-size (rect/rect (* i w) (* j h) w h) 0.95)
                 assoc :fill (color i j)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "none"}
            (apply list (shapes))))

(sketch/definition color-mapping
  {:created-at "2022-05-02"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :color-mapping)
              "sketch-host"))

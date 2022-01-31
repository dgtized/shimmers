(ns shimmers.sketches.angle-of-ascent
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.core :as g]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn lines [{p :p [w h] :size} f n]
  (for [x (range 0 n)]
    (-> (gl/line2 (gv/vec2 (* w (f (/ x n))) 0)
                  (gv/vec2 (* w (f (/ x n))) h))
        (g/translate p))))

(defn shapes []
  (let [a (rect/rect (rv 0.1 0.75) (rv 0.45 0.9))
        b (rect/rect (rv 0.325 0.425) (rv 0.675 0.575))
        c (rect/rect (rv 0.55 0.1) (rv 0.9 0.25))]
    (concat [a b c]
            (lines a (fn [x] (- 1 (Math/pow x 2))) 25)
            (lines b identity 25)
            (lines c (fn [x] (Math/pow x 2)) 25))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (apply list (shapes))))

(sketch/definition angle-of-ascent
  {:created-at "2022-01-31"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :angle-of-ascent)
              "sketch-host"))

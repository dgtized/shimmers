(ns shimmers.sketches.bold-moves
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn circles []
  (let [pos (dr/rand-nth [0.3 0.5 0.7])]
    (for [c [(gc/circle (rv pos 0.0) (* height 0.4))
             (gc/circle (rv (- 1.0 pos) 1.0) (* height 0.4))]]
      (csvg/group {:fill (csvg/hsl 0.0 0.3 0.4 1.0)} c))))

(defn triangles []
  (let [n (dr/rand-nth [2 3 4 5])]
    (for [_ (range n)]
      (csvg/group
        {:fill (csvg/hsl 0.35 0.4 0.4 1.0)}
        (-> (rv (dr/random 0.2 0.8)
                (dr/random 0.2 0.8))
            (gc/circle (* 0.15 height))
            (triangle/inscribed-equilateral (dr/random eq/TAU)))))))

(defn columns []
  (let [n (dr/rand-nth [5 7 9 11])
        spacing (/ 1.0 n)
        hw (* 0.25 spacing)
        dir (* (dr/rand-nth [-1 1])
               (dr/rand-nth [0.25 0.15 0.1 0.05]))]
    (for [t (range 0.1 0.9 spacing)]
      (csvg/group {:fill (csvg/hsl 0.6 0.3 0.4 1.0)}
        (-> (rect/rect (rv (- t hw) 0.25)
                       (rv (+ t hw) 0.75))
            (g/translate (gv/vec2 0.0 (dr/gaussian 0.0 (/ width 30))))
            (geometry/rotate-around-centroid dir))))))

(defn shapes []
  (dr/shuffle
   (concat (circles)
           (triangles)
           (columns))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 2.0}
     (shapes))))

(sketch/definition bold-moves
  {:created-at "2023-01-14"
   :type :svg
   :tags #{:genuary2023}}
  (ctrl/mount (view-sketch/page-for scene :bold-moves)
              "sketch-host"))

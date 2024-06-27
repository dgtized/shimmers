(ns shimmers.sketches.cube-rotations
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg :as csvg]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.aabb :as aabb]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn projection [[w h] [x y z]]
  (let [perspective (* w 0.8)
        scale (/ perspective (+ perspective z))
        origin (tm/* (gv/vec2 w h) 0.5)]
    (-> (gv/vec2 x y)
        (tm/* scale)
        (tm/+ origin))))

(defn cubes [n size]
  (let [perspective (partial projection size)]
    (for [t (butlast (tm/norm-range n))
          :let [cube (-> (aabb/aabb 10)
                         g/center
                         (g/rotate-y (* (/ 1 8) eq/TAU))
                         (g/rotate-x (* (/ 1 8) eq/TAU))
                         (g/rotate-z (* t eq/TAU)))]]
      (for [[a b] (g/edges cube)]
        (gl/line2 (perspective a) (perspective b))))))

(defn k-swaps [k xs]
  (let [n (count xs)]
    (letfn [(swaps [xs]
              (let [i (dr/random-int n)
                    j (dr/random-int n)]
                (assoc xs i (nth xs j)
                       j (nth xs i))))]
      (nth (iterate swaps (vec xs)) k))))

(defn grid []
  (let [margin 0
        r 8
        c 12
        bounds (rect/rect margin margin (- width margin) (- height margin))]
    (map (fn [bbox cube]
           (gu/fit-all-into-bounds (g/scale-size bbox 0.9) cube))
         (g/subdivide bounds {:cols c :rows r})
         (k-swaps 3 (cubes (* r c) [(/ width c) (/ height r)])))))

(defn scene []
  (csvg/svg-timed {:id "scene"
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.8}
    (grid)))

(defn ui-controls []
  [kb/kb-action "alt-s" #(svg-export/download "scene" "cube-rotations")])

(sketch/definition cube-rotations
  {:created-at "2022-01-09"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :cube-rotations ui-controls)))

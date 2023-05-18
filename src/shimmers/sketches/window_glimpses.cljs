(ns shimmers.sketches.window-glimpses
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.core :as g]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn generate-box [{[width height] :size} existing]
  (let [w (dr/random-int (* 0.04 width) (int (* 0.44 width)))
        h (dr/random-int (* 0.04 height) (int (* 0.44 height)))
        box (rect/rect (dr/random-int 0 (- width w))
                       (dr/random-int 0 (- height h))
                       w h)]
    (if (some (fn [s] (collide/overlaps? (g/scale-size box 1.1) s)) existing)
      existing
      (conj existing box))))

(defn shapes [bounds n]
  (->> []
       (iterate (partial generate-box bounds))
       (take-while (fn [s] (< (count s) n)))
       last))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height) 16)))

(sketch/definition window-glimpses
  {:created-at "2023-05-18"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :window-glimpses)))

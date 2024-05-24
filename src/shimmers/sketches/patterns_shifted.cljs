(ns shimmers.sketches.patterns-shifted
  (:require
   [clojure.set :as set]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.rect :as rect]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-shape [size]
  (dr/weighted
   {(triangle/inscribed-equilateral (gv/vec2) size 0) 2
    (rect/rect size) 0}))

(defn shapes [size shape n]
  (loop [shapes [shape] faces (set (g/edges shape))]
    (if (>= (count shapes) n)
      shapes
      (let [[fp fq] (dr/rand-nth (into [] faces))
            mid (tm/mix fp fq 0.5)
            structure-face (g/normal (tm/- fq fp))
            ;; TODO: avoid overlap
            shape (g/rotate (random-shape size) (g/heading (tm/- structure-face)))
            pos (tm/- mid (tm/* structure-face 0.33))
            shape' (g/translate shape pos)]
        ;; TODO remove connecting edge from new shape
        (recur (conj (conj shapes shape') (gc/circle mid 2.0))
               (set/union faces (set (g/edges shape'))))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes (* 0.05 height)
            (vary-meta (g/center (random-shape (* 0.05 height))
                                 (rv 0.5 0.5))
                       assoc :stroke-width 2.0)
            64)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :patterns-shifted]
     [:div.readable-width]]))

(sketch/definition patterns-shifted
  {:created-at "2024-05-23"
   :tags #{}
   :type :svg}
  (ctrl/mount page))

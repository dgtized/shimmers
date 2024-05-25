(ns shimmers.sketches.patterns-shifted
  (:require
   [clojure.set :as set]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(def bounds (rect/rect 0 0 width height))
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-shape [size]
  (dr/weighted
   {(triangle/inscribed-equilateral (gv/vec2) size 0) 2
    (rect/rect size) 0}))

(defn gen-shapes [size shape n]
  (loop [structure [shape]
         faces (set (g/edges shape))
         annotation []]
    (if (or (empty? faces) (>= (count structure) n))
      {:structure structure :annotation annotation :faces faces}
      (let [face (dr/rand-nth (into [] faces))
            [fp fq] face
            mid (tm/mix fp fq 0.5)
            structure-face (g/normal (tm/- fq fp))
            shape (g/rotate (random-shape size) (g/heading (tm/- structure-face)))
            pos (tm/- mid (tm/* structure-face 0.4))
            shape' (g/translate shape pos)
            edges (drop 1 (sort-by (fn [[p q]] (g/dist-squared mid (tm/mix p q 0.5)))
                                   (g/edges shape')))]
        (if (or (not (collide/bounded? bounds shape'))
                (some (fn [s] (collide/overlaps? s shape')) structure))
          (recur structure
                 (disj faces face)
                 annotation)
          (recur (conj structure shape')
                 (set/union (disj faces face) (set edges))
                 (conj annotation (gc/circle mid 2.0))))))))

(defn shapes []
  (let [starting (vary-meta (g/center (random-shape (* 0.05 height))
                                      (rv 0.5 0.5))
                            assoc :stroke-width 2.0)
        {:keys [structure annotation faces]}
        (gen-shapes (* 0.05 height)
                    starting
                    32)]
    (conj (concat structure annotation)
          (csvg/group {:stroke-width 2.0}
            (mapv (fn [[p q]] (gl/line2 p q)) faces)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes)))

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

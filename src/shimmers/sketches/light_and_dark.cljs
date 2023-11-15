(ns shimmers.sketches.light-and-dark
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn diagonal? [v]
  (let [[x y] v]
    (and (> (abs x) 0) (> (abs y) 0))))

(defn polygon-from-pair [bounds left right]
  (let [[a b] (g/vertices left)
        [c d] (g/vertices right)]
    ;; trying to add in corners if missing
    (gp/polygon2 (cond-> (if (diagonal? (tm/- d b))
                           [a b (apply min-key (partial g/dist b) (g/vertices bounds)) d c]
                           [a b d c])
                   (diagonal? (tm/- a c))
                   (conj (apply min-key (partial g/dist c) (g/vertices bounds)))))))

(defn shapes [bounds angle cuts]
  (let [lines (sort-by (fn [line] (:x (g/centroid line)))
                       (clip/hatch-rectangle bounds (/ (g/width bounds) (inc cuts)) angle))]
    (for [[left right] (partition 2 2 lines)]
      (vary-meta (polygon-from-pair bounds left right)
                 assoc :fill "black"))))

(defn scene []
  (let [bounds (rect/rect 0 0 width height)
        angle (* eq/TAU (dr/random 0.10 0.40))
        cuts (dr/rand-nth (sm/primes-between 4 20))]
    (csvg/svg-timed {:width width
                     :height height
                     :stroke "black"
                     :fill "white"
                     :stroke-width 1.0}
      (shapes bounds angle cuts))))

(sketch/definition light-and-dark
    {:created-at "2023-11-13"
     :tags #{}
     :type :svg}
  (ctrl/mount (view-sketch/static-page scene :light-and-dark)))

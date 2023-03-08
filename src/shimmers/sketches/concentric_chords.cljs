(ns shimmers.sketches.concentric-chords
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
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

(defn closest-dist [bounds point]
  (g/dist point (g/closest-point bounds point)))

(defn max-circle-in-bounds [bounds point]
  (let [p (g/unmap-point bounds point)]
    (gc/circle p (closest-dist bounds p))))

(defn pack-overlap-circles [bounds n]
  (let [legal-candidate
        (fn [circles]
          (let [p (gv/vec2 (dr/random 0.1 0.9) (dr/random 0.1 0.9))
                candidate (g/scale-size (max-circle-in-bounds bounds p) 0.66)]
            (when-not (some
                       (fn [c] (> (geometry/percent-circle-overlap c candidate) 0.03))
                       circles)
              (conj circles candidate))))]
    (reduce (fn [circles _]
              (or (cs/retry 30 (partial legal-candidate circles))
                  circles))
            []
            (range n))))

(defn shapes [bounds]
  (pack-overlap-circles bounds 11))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"
              :stroke-width 1.0}
     (shapes (g/scale-size (rect/rect 0 0 width height) 0.98)))))

(sketch/definition concentric-chords
  {:created-at "2023-03-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :concentric-chords)
              "sketch-host"))

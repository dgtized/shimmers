(ns shimmers.sketches.curvature-of-space
  (:require
   [shimmers.algorithm.space-colonization :as colonize]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn build-tree [bounds source attractors]
  (-> {:bounds bounds
       :branches [source]
       :attractors attractors
       :influence-distance 48
       :prune-distance 6
       :segment-distance 3
       :snap-theta 0}
      colonize/create-tree
      colonize/grow-tree))

(defn tree->segments [{:keys [branches]}]
  (for [branch branches
        :let [parent (:parent branch)]
        :when parent]
    (gl/line2 (:position (nth branches parent))
              (:position branch))))

(defn gen-points [n]
  (let [circle (gc/circle (rv 0.5 0.5) (* height 0.45))]
    (repeatedly n #(geometry/random-point-in-circle circle))))

(defn shapes [bounds]
  (->> (gen-points 256)
       (build-tree bounds (colonize/make-root (rv 0.5 0.5) (dr/randvec2)))
       tree->segments))

(defn scene []
  (let [bounds (rect/rect 0 0 width height)]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :fill "white"
               :stroke-width 1.0}
              (shapes bounds))))

(sketch/definition curvature-of-space
  {:created-at "2022-01-18"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :curvature-of-space)
              "sketch-host"))

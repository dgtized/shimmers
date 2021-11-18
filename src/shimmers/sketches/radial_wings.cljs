(ns shimmers.sketches.radial-wings
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.algorithm.lines :as lines]
   [shimmers.math.vector :as v]
   [thi.ng.geom.polygon :as gp]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn rp [r theta]
  (v/polar (* r 0.5 height) theta))

(defn shapes []
  (->> [(gp/polygon2 [(rp 0 0) (rp 0.5 0.5) (rp 0.5 0.8)])]
       (mapv #(g/translate % (rv 0.5 0.5)))))

;; FIXME: handle large gaps and overlapping lines
(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :stroke-width 1.0}
            (for [[i shape] (map-indexed vector (shapes))]
              (vary-meta shape assoc :key (str "l" i)))))

(defn page []
  [:div
   [:div#canvas-host.canvas-frame [scene]]
   [:p.center (view-sketch/generate :radial-wings)]])

(sketch/definition radial-wings
  {:created-at "2021-11-15"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "sketch-host"))

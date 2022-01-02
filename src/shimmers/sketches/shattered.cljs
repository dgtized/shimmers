(ns shimmers.sketches.shattered
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.geometry :as geometry]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (->> [(rect/rect (rv 0.1 0.1) (rv 0.4 0.9))
        (rect/rect (rv 0.6 0.1) (rv 0.9 0.9))]
       (mapcat #(geometry/shatter % 100))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition shattered
  {:created-at "2022-01-01"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :shattered)
              "sketch-host"))

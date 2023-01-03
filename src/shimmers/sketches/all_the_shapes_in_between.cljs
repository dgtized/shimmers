(ns shimmers.sketches.all-the-shapes-in-between
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  [])

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 1.0}
             (shapes))))

(sketch/definition all-the-shapes-in-between
  {:created-at "2023-01-02"
   :type :svg
   :tags #{:genuary2023}}
  (ctrl/mount (view-sketch/page-for scene :all-the-shapes-in-between)
              "sketch-host"))

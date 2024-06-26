(ns shimmers.sketches.template.svg
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.svg-export :as svg-export]
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
  (csvg/svg-timed {:id "scene"
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :template.svg]
     [kb/kb-action "alt-s" #(svg-export/download "scene" "template.svg")]
     [:div.readable-width]]))

(sketch/definition template.svg
  {:created-at "2024-"
   :tags #{}
   :type :svg}
  (ctrl/mount page))

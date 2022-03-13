(ns shimmers.sketches.random-point-field
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce ui-state (ctrl/state {:mode :random-points
                               :n-points 512}))
(def modes {:random-points rp/random-points
            :random-cells rp/random-cells
            :random-cell-jitter rp/random-cell-jitter} )

(defn scene [points]
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (svg/group {:fill "black"}
                       (map (fn [p] (gc/circle p 1.5)) points))))

(defn page []
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.99)
        {:keys [mode n-points]} @ui-state
        point-cloud (get modes mode)
        points (point-cloud bounds n-points)]
    [:div
     [:div.canvas-frame [scene points]]
     [:div.explanation
      [:div.flexcols
       [:div.width {:style {:width "15em"}}
        [view-sketch/generate :random-point-field]
        [:p "Various approaches of generating a random set of points in a boundary."]
        [:p (str "Generated " (count points) " points")]]
       [:div
        [:h4 "Controls"]
        (ctrl/change-mode ui-state (keys modes))
        (ctrl/numeric ui-state "Generated Points" [:n-points] [2 1024 1])]]]]))

(sketch/definition random-point-field
  {:created-at "2022-05-12"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))

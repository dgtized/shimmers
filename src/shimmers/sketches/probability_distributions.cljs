(ns shimmers.sketches.probability-distributions
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 400)
(def height 50)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn histogram [samples]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "black"
             :stroke-width 0.5}
    (for [x samples]
      (rect/rect (- (* x width) 0.5) 0
                 1.0 height))))

(defn approaches []
  [{:title "Uniform Random"
    :generator #(dr/random)}
   {:title "Gaussian {:mu 0.5 :sd 0.15}"
    :generator #(dr/gaussian 0.5 0.15)}
   {:title "Gaussian {:mu 0.5 :sd 0.08}"
    :generator #(dr/gaussian 0.5 0.08)}])

(defn page []
  [:div
   (into [:div]
         (for [{:keys [title generator]} (approaches)]
           [:<>
            [:h3.center title]
            [:div.canvas-frame
             [histogram (repeatedly 200 generator)]]]))
   [:div.explanation.contained
    [:div.center
     [:div [view-sketch/generate :probability-distributions]]]]])

(sketch/definition probability-distributions
  {:created-at "2023-01-31"
   :type :svg
   :tags #{:debug}}
  (ctrl/mount page "sketch-host"))

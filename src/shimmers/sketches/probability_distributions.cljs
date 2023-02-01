(ns shimmers.sketches.probability-distributions
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [shimmers.common.ui.debug :as debug]))

(def width 250)
(def height 40)
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
   {:title "Gaussian"
    :params {:mu 0.5 :sd 0.3}
    :generator #(dr/gaussian 0.5 0.3)}
   {:title "Gaussian"
    :params {:mu 0.5 :sd 0.15}
    :generator #(dr/gaussian 0.5 0.15)}
   {:title "Gaussian"
    :params {:mu 0.5 :sd 0.075}
    :generator #(dr/gaussian 0.5 0.075)}
   {:title "Pareto"
    :params {:scale 0.01 :shape 0.8}
    :generator #(dr/pareto 0.01 0.8)}
   {:title "Pareto"
    :params {:scale 0.01 :shape 1.05}
    :generator #(dr/pareto 0.01 1.05)}
   {:title "Pareto"
    :params {:scale 0.01 :shape 1.5}
    :generator #(dr/pareto 0.01 1.5)}
   {:title "Pareto"
    :params {:scale 0.01 :shape 2.0}
    :generator #(dr/pareto 0.01 2.0)}
   ])

(defn page []
  [:div
   [:p]
   [:h3.contained.center "Visualize Probability Distributions"]
   (into [:table.contained]
         (for [{:keys [title params generator]} (approaches)
               :let [samples (repeatedly 256 generator)]]
           [:tr
            [:td title
             [:br]
             [:pre [:code (str params)]]]
            [:td
             [:div.canvas-frame
              [histogram samples]]]
            [:td
             (debug/pre-edn (dr/summary-stats samples) {:width 50})]]))
   [:div.explanation.contained
    [:div.center
     [:div [view-sketch/generate :probability-distributions]]]]])

(sketch/definition probability-distributions
  {:created-at "2023-01-31"
   :type :svg
   :tags #{:debug}}
  (ctrl/mount page "sketch-host"))

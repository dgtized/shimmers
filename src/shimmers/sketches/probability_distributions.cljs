(ns shimmers.sketches.probability-distributions
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 250)
(def height 40)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn scatter-1d [samples]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "black"
             :stroke-width 0.5}
    (for [x samples]
      (rect/rect (- (* x width) 0.5) 0
                 1.0 height))))

(defn histogram [samples]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "navy"
             :stroke-width 0.5}
    (let [freqs (frequencies (map (comp int (partial * 10)) samples))
          buckets (reduce-kv (fn [b k v]
                               (update b
                                       (cond (< k 0) -1
                                             (> k 10) 11
                                             :else k) (fnil + 0)
                                       v))
                             {} freqs)]
      (for [[bucket value] (sort-by first buckets)]
        (rect/rect (* width (/ (inc bucket) 12))
                   (* height (- 1.0 (min 1.0 (/ (float value) 128))))
                   (* width (/ 1 12))
                   height)))))

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
   {:title "Gaussian Between"
    :params {:mu 0.5 :sd 0.3 :min 0.0 :max 1.0}
    :generator #(dr/gaussian-between 0.5 0.4 0.0 1.0)}
   {:title "Gaussian Between"
    :params {:mu 0.25 :sd 0.3 :min 0.0 :max 1.0}
    :generator #(dr/gaussian-between 0.25 0.5 0.0 1.0)}
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
   {:title "Pareto (clamped)"
    :params {:scale 0.01 :shape 0.5}
    :generator #(tm/clamp01 (dr/pareto 0.01 0.5))}
   {:title "Pareto (clamped + noise)"
    :params {:scale 0.01 :shape 0.5}
    :generator #(tm/clamp01 (+ (dr/pareto 0.01 0.5) (dr/gaussian 0.0 0.02)))}
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
              [scatter-1d samples]
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
  (ctrl/mount page))

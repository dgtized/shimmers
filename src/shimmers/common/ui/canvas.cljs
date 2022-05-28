(ns shimmers.common.ui.canvas
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]))

;; See also https://github.com/reagent-project/reagent-cookbook/tree/master/recipes/canvas-fills-div
(defn sizing-attributes [width height attributes]
  (-> attributes
      (merge {:width width :height height})
      (assoc-in [:style :width] (str width "px"))
      (assoc-in [:style :height] (str width "px"))))

;; TODO: how to make this lightweight enough to combine with devcards like visual tests?
;; As example, if I wanted a micro visual demo of contains-box?/contains-entity?
(defn animated-canvas [canvas-state attributes render-frame-fn]
  (let [cancel-animation (atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (reset! cancel-animation
                (render-frame-fn this (rdom/dom-node this) canvas-state)))
      :component-will-unmount
      (fn [_] (@cancel-animation))
      :reagent-render
      (fn [_]
        (let [{:keys [width height]} @canvas-state]
          [:canvas (sizing-attributes width height attributes)]))})))

(defn canvas-frame [attrs canvas-state render-frame-fn]
  [animated-canvas canvas-state attrs render-frame-fn])

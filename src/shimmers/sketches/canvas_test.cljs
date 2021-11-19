(ns shimmers.sketches.canvas-test
  (:require [helins.canvas :as cv]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.sketch :as sketch :include-macros true]))

(def canvas-state (r/atom {:width 400 :height 300}))

(defn canvas [attributes render-frame-fn]
  (let [cancel-animation (atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (println "mounting")
        (reset! cancel-animation
                (render-frame-fn this (rdom/dom-node this))))
      :component-will-unmount
      (fn [_]
        (println "canceling")
        (@cancel-animation))
      :reagent-render
      (fn [_]
        [:canvas attributes])})))

(defn set-size! [width height]
  (swap! canvas-state assoc :width width :height height))

;; not quite updating the canvas size?
(comment (set-size! 800 600))

(defn canvas-frame [render-frame-fn]
  (let [{:keys [width height]} @canvas-state]
    (canvas {:class "canvas-frame"
             :width width :height height
             :style {:width (str width "px")
                     :height (str height "px")}}
            render-frame-fn)))

(defn draw-frame [_ canvas]
  (let [ctx (cv/high-dpi (.getContext canvas "2d"))]
    (cv/on-frame
     (fn [ts]
       (if (<= (mod ts 250) 10)
         (let [{:keys [width height]} @canvas-state
               size 50
               margin 10]
           (-> ctx
               (cv/color-fill "white")
               (cv/rect-fill 0 0 width height)
               (cv/color-fill "black")
               (cv/rect-fill (dr/random-int margin (- width size margin))
                             (dr/random-int margin (- height size margin))
                             (min size width) (min size height))))
         ts)))))

(defn page []
  [:div
   [:div {:style {:float "left"}}
    [:h4 "Frame 1"]
    [canvas-frame draw-frame]]
   [:div {:style {:float "right"}}
    [:h4 "Frame 2"]
    [canvas-frame draw-frame]]
   [:div {:style {:clear :both}}]
   [:p.explanation.readable-width
    "Experimenting with an alternative Canvas renderer from Quil. As it can
   mount as a React component, it's easier to host multiple in a single
   sketch."]])

(sketch/definition canvas-test
  {:created-at "2021-11-18"
   :type :canvas}
  (ctrl/mount page "sketch-host"))

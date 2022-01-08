(ns shimmers.sketches.canvas-test
  (:require
   [helins.canvas :as cv]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn sizing-attributes [width height attributes]
  (merge
   {:width width :height height
    :style {:width (str width "px")
            :height (str height "px")}}
   attributes))

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

(defn set-size! [canvas-state width height]
  (swap! canvas-state assoc :width width :height height))

;; TODO: not quite updating the canvas size dynamically?
(defn toggle-size [canvas-state]
  (let [{:keys [width]} @canvas-state]
    (if (= width 200)
      (set-size! canvas-state 300 300)
      (set-size! canvas-state 200 200))))

(defonce canvas-state (r/atom {:width 200 :height 200}))
(def telemetry (r/atom {}))

(defn canvas-frame [canvas-state render-frame-fn]
  [animated-canvas canvas-state {:class "canvas-frame"} render-frame-fn])

(defn update-box [state bounds]
  (let [{:keys [pos vel size]} state
        new-pos (tm/+ pos vel)]
    ;; TODO: handle out of bounds case on resize?
    (if (geometry/contains-box? bounds (rect/rect new-pos (tm/+ new-pos (gv/vec2 size size))))
      (assoc state
             :pos new-pos
             :vel vel)
      (let [x (:x new-pos)
            face (if (or (< x (rect/left bounds))
                         (> (+ x size) (rect/right bounds)))
                   (gv/vec2 0 1)
                   (gv/vec2 1 0))
            new-vel (g/reflect vel face)]
        (assoc state
               :pos pos
               :vel new-vel)))))

;; TODO: Can the boxes bounce into the other canvas without sharing state?
(defn draw-frame [id]
  (fn [_ canvas canvas-state]
    (let [ctx (cv/high-dpi (.getContext canvas "2d"))
          {:keys [width height]} @canvas-state
          margin 10
          size 50
          box-state (atom {:size size
                           :pos (gv/vec2 (dr/random-int margin (- width size margin))
                                         (dr/random-int margin (- height size margin)))
                           :vel (tm/normalize (gv/randvec2) 2.0)})]
      (cv/on-frame
       (fn [_]
         (let [{:keys [width height]} @canvas-state
               bounds (rect/rect 0 0 width height)
               {:keys [pos size] :as box-state'} (update-box @box-state bounds)
               [x y] pos]
           (swap! telemetry assoc id pos)
           (reset! box-state box-state')
           (-> ctx
               (cv/color-fill "white")
               (cv/rect-fill 0 0 width height)
               (cv/color-fill "black")
               (cv/rect-fill x y size size))))))))

(defn page []
  [:div
   [:div {:style {:float "left"}}
    [:h4 "Frame 1"]
    [canvas-frame canvas-state (draw-frame :a)]]
   [:div {:style {:float "right"}}
    [:h4 "Frame 2"]
    [canvas-frame canvas-state (draw-frame :b)]]
   [:div {:style {:clear :both}}]
   [:div.explanation
    [:p.readable-width
     "Experimenting with an alternative Canvas renderer from Quil. As it can
   mount as a React component, it's easier to host multiple in a single
   sketch."]
    [:button {:on-click #(toggle-size canvas-state)} "Toggle Size"]
    (debug/display canvas-state)
    (debug/display telemetry)]])

(sketch/definition canvas-test
  {:created-at "2021-11-18"
   :type :canvas}
  (ctrl/mount page "sketch-host"))

(ns shimmers.sketches._canvas-template
  (:require
   [reagent.core :as r]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup [_cv]
  {})

(defn update-state [state _dims _ms]
  state)

(defn draw [_cv ctx [_width _height] ms]
  (let [_t (* 0.001 ms)]
    ctx))

(defn page []
  (let [canvas-state
        (r/atom {:width 800
                 :height 600
                 :setup #'setup
                 :update #'update-state
                 :draw #'draw})
        toggle-fs
        (fn [] (canvas/toggle-full-screen! canvas-state {:width-pct 0.7}))
        attributes {:class "canvas-frame" :on-double-click toggle-fs}]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]])))

(sketch/definition _canvas-template
  {:created-at "2023-"
   :type :canvas
   :tags #{}}
  (ctrl/mount (page) "sketch-host"))

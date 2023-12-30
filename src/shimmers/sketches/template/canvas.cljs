(ns shimmers.sketches.template.canvas
  (:require
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
  (let [{:keys [canvas-state attributes]}
        (canvas/make-state
         {:width 800
          :height 600
          :setup #'setup
          :update #'update-state
          :draw #'draw}
         {:width-pct 0.7})]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]])))

(sketch/definition template.canvas
  {:created-at "2023-"
   :tags #{}
   :type :canvas}
  (ctrl/mount (page)))

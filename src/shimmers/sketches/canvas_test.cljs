(ns shimmers.sketches.canvas-test
  (:require [helins.canvas :as cv]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.math.deterministic-random :as dr]))

(defn page []
  [:div
   [ctrl/canvas {:class "canvas-frame"
                 :width 800 :height 600
                 :style {:width "800px" :height "600px"}}
    (fn [_ canvas]
      (let [ctx (cv/high-dpi (.getContext canvas "2d"))]
        (-> ctx
            (cv/color-fill "black")
            (cv/rect-fill (dr/random-int 50 550) (dr/random-int 50 350)
                          200 200))))]])

(sketch/definition canvas-test
  {:created-at "2021-11-18"
   :type :canvas}
  (ctrl/mount page "sketch-host"))

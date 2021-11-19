(ns shimmers.sketches.canvas-test
  (:require [helins.canvas :as cv]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.math.deterministic-random :as dr]))

(defn canvas-frame [width height render-frame-fn]
  (ctrl/canvas {:class "canvas-frame"
                :width width :height height
                :style {:width (str width "px")
                        :height (str height "px")}}
               render-frame-fn))

(defn draw-frame [_ canvas]
  (let [ctx (cv/high-dpi (.getContext canvas "2d"))]
    (-> ctx
        (cv/color-fill "black")
        (cv/rect-fill (dr/random-int 50 550) (dr/random-int 50 350)
                      200 200))))

(defn page []
  [:div [canvas-frame 800 600 draw-frame]])

(sketch/definition canvas-test
  {:created-at "2021-11-18"
   :type :canvas}
  (ctrl/mount page "sketch-host"))

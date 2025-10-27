(ns shimmers.sketches.bias-gain
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.bias-gain :as mbg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [s t]
  (let [points (mapv (fn [x] (rv x (- 1.0 (mbg/bias-gain x s t))))
                     (range 0 1 0.0025))]
    [(csvg/path (csvg/segmented-path points) {})]))

(defonce ui-state (ctrl/state {:s 1.0 :t 0.5}))

(defn scene [{:keys [scene-id]}]
  (let [{:keys [s t]} @ui-state]
    (csvg/svg-timed {:id scene-id
                     :width width
                     :height height
                     :stroke "black"
                     :fill "white"
                     :stroke-width 0.5}
                    (shapes s t))))

(defn ui-controls []
  [:div
   [ctrl/numeric ui-state "S" [:s] [0.0001 100.0 0.0001]]
   [ctrl/numeric ui-state "T" [:t] [0.0 1.0 0.0001]]])

(sketch/definition bias-gain
  {:created-at "2025-10-27"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page (assoc sketch-args :explanation ui-controls) scene)))

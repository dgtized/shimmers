(ns shimmers.sketches.bias-gain
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.bias-gain :as mbg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [s t]
  (let [w 0.0075
        points (mapv (fn [x] (rv x (- 1.0 (mbg/bias-gain x s t))))
                     (range 0 1 0.0025))]
    [(csvg/path (csvg/segmented-path points) {})
     (csvg/group {:stroke-width 0.66}
       (cs/midsection
        (for [x (range 0 1 0.1)]
          (gl/line2 (rv x (- 1.0 w)) (rv x 1.0)))))
     (csvg/group {:stroke-width 0.66}
       (cs/midsection
        (for [y (range 0 1 0.1)]
          (gl/line2 (rv 0.0 y) (rv w y)))))]))

(defonce ui-state (ctrl/state {:s 1.0 :t 0.5}))

(defn scene [{:keys [scene-id]}]
  (let [{:keys [s t]} @ui-state]
    (csvg/svg-timed {:id scene-id
                     :width width
                     :height height
                     :stroke "black"
                     :fill "white"
                     :stroke-width 1.5}
      (shapes s t))))

(defn ui-controls []
  [:div.contained
   [ctrl/numeric ui-state "S" [:s] [0.01 100.0 0.01]]
   [ctrl/numeric ui-state "T" [:t] [0.0 1.0 0.01]]])

(sketch/definition bias-gain
  {:created-at "2025-10-27"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page (assoc sketch-args :explanation ui-controls) scene)))

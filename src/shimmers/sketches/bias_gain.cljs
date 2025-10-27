(ns shimmers.sketches.bias-gain
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.bias-gain :as mbg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn plot [s t]
  (mapv (fn [x] (rv x (- 1.0 (mbg/bias-gain x s t))))
        (range 0 1 0.0025)))

(defn shapes [s t]
  (let [w 0.0075]
    [(csvg/path (csvg/segmented-path (plot (+ s 0.25) t))
                {:stroke "#0000ff" :stroke-width 0.5})
     (csvg/path (csvg/segmented-path (plot (max 0.01 (- s 0.25)) t))
                {:stroke "#0000ff" :stroke-width 0.5})
     (csvg/path (csvg/segmented-path (plot s (tm/clamp01 (- t 0.125))))
                {:stroke "#ff0000" :stroke-width 0.5})
     (csvg/path (csvg/segmented-path (plot s (tm/clamp01 (+ t 0.125))))
                {:stroke "#ff0000" :stroke-width 0.5})
     (csvg/path (csvg/segmented-path (plot s t)) {:stroke-width 1.5})
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
                     :fill "none"
                     :stroke-width 0.5}
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

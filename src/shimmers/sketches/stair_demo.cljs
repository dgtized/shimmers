(ns shimmers.sketches.stair-demo
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.stair :as ms]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn plot
  ([f]
   (plot f {:resolution 0.0025}))
  ([f {:keys [resolution]}]
   (mapv (fn [x] (rv x (- 1.0 (f x))))
         (range 0 1 resolution))))

(defn shapes [k]
  (let [w 0.0075]
    [(csvg/path (csvg/segmented-path (plot (fn [x] (ms/staircase k x))))
                {:stroke-width 1.5})
     (csvg/group {:stroke-width 0.66}
       (cs/midsection
        (for [x (range 0 1 0.1)]
          (gl/line2 (rv x (- 1.0 w)) (rv x 1.0)))))
     (csvg/group {:stroke-width 0.66}
       (cs/midsection
        (for [y (range 0 1 0.1)]
          (gl/line2 (rv 0.0 y) (rv w y)))))]))

(defonce ui-state (ctrl/state {:k 3.0}))

(defn scene [{:keys [scene-id]}]
  (let [{:keys [k]} @ui-state]
    (csvg/svg-timed {:id scene-id
                     :width width
                     :height height
                     :stroke "black"
                     :fill "none"
                     :stroke-width 0.5}
      (shapes k))))

(defn ui-controls []
  [:div.contained
   [ctrl/numeric ui-state "k" [:k] [0.01 32.0 0.01]]])

(sketch/definition stair-demo
  {:created-at "2025-11-17"
   :tags #{:demo}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args ui-controls scene)))

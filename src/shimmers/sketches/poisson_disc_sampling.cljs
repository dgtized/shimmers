(ns shimmers.sketches.poisson-disc-sampling
  "https://www.youtube.com/watch?v=flQgnCUxHlw"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.poisson-disc-sampling :as pds]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.sketch :as sketch :include-macros true]))

(def ui-state
  (ctrl/state {:radius 8 :samples 10}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [radius samples]} @ui-state]
    (pds/init (cq/screen-rect 0.8) radius samples 10)))

(defn update-state [{:keys [n] :as state}]
  (cs/iterate-cycles n pds/fill-step state))

(defn draw [{:keys [active grid]}]
  (q/background 255)
  (q/stroke 0)
  (q/stroke-weight 2)
  (doseq [[x y] (vals grid)]
    (q/point x y))
  (q/stroke-weight 4)
  (q/stroke 0 0.5 0.5)
  (doseq [[x y] active]
    (q/point x y)))

(defn ui-controls []
  (ctrl/container
   (ctrl/slider ui-state (fn [v] (str "Min Separation: " v))
                [:radius] [2 16 1])
   ;; Is this parameter even worth tuning?
   (ctrl/slider ui-state (fn [v] (str "Samples per Location: " v))
                [:samples] [5 50 1])))

(sketch/defquil poisson-disc-sampling
  :created-at "2021-06-30"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :tags #{:demo}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

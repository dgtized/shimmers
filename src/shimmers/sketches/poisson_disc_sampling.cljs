(ns shimmers.sketches.poisson-disc-sampling
  "https://www.youtube.com/watch?v=flQgnCUxHlw"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.poisson-disc-sampling :as pds]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]))

(def ui-state
  (ctrl/state
   {:variable false
    :radius 8
    :samples 10}))

(defn sqrt-dist-from [origin]
  (fn [p] (Math/sqrt (g/dist p origin))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [variable radius samples]} @ui-state]
    (if variable
      (pds/init-dynamic (cq/screen-rect 0.8) samples 10
                        [radius (* 4 radius)]
                        (sqrt-dist-from (cq/rel-vec 0.5 0.5)))
      (pds/init (cq/screen-rect 0.8) samples 10 radius))))

(defn update-state [{:keys [n] :as state}]
  (cs/iterate-cycles n pds/fill-step state))

(defn draw [{:keys [active points]}]
  (q/background 255)
  (q/stroke 0)
  (q/stroke-weight 2)
  (doseq [[x y] points]
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
                [:samples] [5 50 1])
   (ctrl/checkbox ui-state "Variable Radius" [:variable])))

(sketch/defquil poisson-disc-sampling
  :created-at "2021-06-30"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :tags #{:demo}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

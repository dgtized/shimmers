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
            [shimmers.view.sketch :as view-sketch]
            [thi.ng.geom.core :as g]))

(def modes [:fixed :variable])

(defonce ui-state
  (ctrl/state
   {:mode :fixed
    :radius 8
    :cycles-per-frame 10
    :radius-fn :sqrt-distance
    :samples 10}))

(def radius-source
  {:sqrt-distance (fn [p] (/ (g/dist p (cq/rel-vec 0.5 0.5)) (* 0.5 (q/width))))
   :perlin-noise (fn [[x y]]
                   (let [d 0.01]
                     (q/noise (* d x) (* d y))))})

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [mode radius samples radius-fn]} @ui-state
        bounds (cq/screen-rect 0.8)]
    (if (= mode :variable)
      (pds/init-dynamic bounds samples
                        ;; Need a better way to map 0 to 1 to radius
                        [radius (* 4 radius)]
                        (get radius-source radius-fn))
      (pds/init bounds samples radius))))

(defn update-state [state]
  (cs/iterate-cycles (:cycles-per-frame @ui-state)
                     pds/fill-step state))

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

(defn restart []
  (view-sketch/restart-sketch {:id :poisson-disc-sampling}))

(defn ui-controls []
  (ctrl/container
   (ctrl/change-mode ui-state [:fixed :variable] {:on-change restart})
   (ctrl/slider ui-state (fn [v] (str "Min Separation: " v))
                [:radius] [2 32 1])
   (when (= (:mode @ui-state) :variable)
     (ctrl/change-mode ui-state (keys radius-source) {:mode-key :radius-fn}))))

(sketch/defquil poisson-disc-sampling
  :created-at "2021-06-30"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :tags #{:demo}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.sediment
  "Experiment influenced by https://inconvergent.net/2016/shepherding-random-numbers/"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:sand false
               :particle-count 100
               :horizontal-start 5
               :average-window 16
               :acceleration 2}))

(defn ui-controls []
  [:div
   [:h5 "Applies at Restart"]
   (ctrl/slider ui-state (fn [v] (str "Particles: " v))
                [:particle-count] [20 1000 20])
   (ctrl/slider ui-state (fn [v] (str "Y-Axis Start: " v))
                [:horizontal-start] [0 10 1])
   [:h5 "Applies Immediately"]
   (ctrl/slider ui-state (fn [v] (str "Averaging Window: " v))
                [:average-window] [4 128 4])
   (ctrl/slider ui-state (fn [v] (str "Acceleration: " v))
                [:acceleration] [1 20 1])
   (ctrl/checkbox ui-state "Sand" [:sand])])

(defrecord Particle [pos prev])

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [particle-count horizontal-start]} @ui-state
        y-start (/ horizontal-start 10)
        dx (/ 1 particle-count)]
    {:particles (for [i (range 0 1 dx)
                      :let [pos (cq/rel-vec i y-start)]]
                  (Particle. pos pos))}))

(defn velocity [{:keys [pos prev]}]
  (:y (tm/- pos prev)))

(defn update-point [{:keys [pos] :as point} surrounding]
  (let [vel (/ (reduce + (map velocity surrounding))
               (count surrounding))
        acc (* (/ (:acceleration @ui-state) 20) (q/random-gaussian))
        vel' (gv/vec2 0 (* 0.995 (+ vel acc)))
        pos' (update (tm/+ pos vel') :y tm/clamp 0 (q/height))]
    (assoc point
           :prev pos
           :pos pos')))

(defn update-particles [particles]
  (cs/map-with-window (:average-window @ui-state) update-point particles))

(defn update-state [state]
  (update state :particles update-particles))

(defn draw [{:keys [particles]}]
  ;; (q/no-loop)
  (let [{:keys [sand]} @ui-state
        radius (/ (q/width) 2 (count particles))]
    (q/ellipse-mode :radius)
    (q/no-fill)
    (q/stroke 0 0.05)
    (q/stroke-weight 0.35)
    (if sand
      (doseq [{:keys [pos]} particles
              :let [[x y] (p/jitter-x pos radius)]]
        (q/ellipse x y radius radius))
      (doseq [segment (partition 4 1 particles)]
        (apply q/curve (mapcat :pos segment))))))

(sketch/defquil sediment
  :created-at "2021-04-08"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [900 600]
  :setup setup
  :update update-state
  ;; :mouse-clicked (fn [state] (q/redraw) state)
  :draw draw
  :middleware [m/fun-mode framerate/mode])

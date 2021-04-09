(ns shimmers.sketches.sediment
  "Experiment influenced by https://inconvergent.net/2016/shepherding-random-numbers/"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defonce ui-state
  (r/atom {:sand false
           :particle-count 100
           :acceleration 2}))

(defn explanation []
  [:div
   [:h5 "Applies at Restart"]
   (ctrl/slider ui-state (fn [v] (str "Particles: " v))
                [:particle-count] [20 1000 20])
   [:h5 "Applies Immediately"]
   (ctrl/slider ui-state (fn [v] (str "Acceleration: " v))
                [:acceleration] [1 20 1])
   (ctrl/checkbox ui-state "Sand" [:sand])])

(defrecord Particle [pos prev])

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [dx (/ 1 (:particle-count @ui-state))]
    {:particles (for [i (range 0 1 dx)
                      :let [pos (gv/vec2 (cq/rel-pos i 0.5))]]
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
  (cs/map-with-window 16 update-point particles))

(defn update-state [state]
  (update state :particles update-particles))

(defn confusion [[x y] r]
  (let [radius (Math/sqrt (* r (rand)))
        alpha (* 2 Math/PI (rand))]
    [(+ x (* radius (Math/cos alpha)))
     (+ y (* radius (Math/sin alpha)))]))

(defn draw [{:keys [particles]}]
  ;; (q/no-loop)
  (q/no-fill)
  (q/stroke 0 0.05)
  (q/stroke-weight 0.5)
  (if (:sand @ui-state)
    (doseq [{:keys [pos]} particles
            :let [[x y] (confusion pos 0.2)]]
      (q/ellipse x y 0.4 0.4))
    (doseq [segment (partition 4 1 particles)]
      (apply q/curve (mapcat :pos segment)))))

(defn ^:export run-sketch []
  ;; 20210408
  (ctrl/mount explanation)
  (q/defsketch sediment
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    ;; :mouse-clicked (fn [state] (q/redraw) state)
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

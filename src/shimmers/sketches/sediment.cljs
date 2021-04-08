(ns shimmers.sketches.sediment
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defrecord Particle [pos prev])

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [dx 0.025]
    {:particles (for [i (range 0 1 dx)
                      :let [pos (gv/vec2 (cq/rel-pos i 0.5))]]
                  (Particle. pos pos))}))

(defn update-particles [particles]
  (for [{:keys [pos prev] :as point} particles]
    (let [vel (:y (tm/- pos prev))
          acc (* 0.1 (q/random-gaussian))
          vel' (gv/vec2 0 (+ vel acc))
          pos' (update (tm/+ pos vel') :y tm/clamp 0 (q/height))]
      (assoc point
             :prev pos
             :pos pos'))))

(defn update-state [state]
  (update state :particles update-particles))

(defn draw [{:keys [particles]}]
  ;; (q/no-loop)
  ;; (q/background 1.0)
  (q/stroke 0 0.5)
  (q/stroke-weight 0.5)
  (doseq [segment (partition 4 1 particles)]
    (apply q/curve (mapcat #(:pos %) segment))))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch sediment
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    ;; :mouse-clicked (fn [state] (q/redraw) state)
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

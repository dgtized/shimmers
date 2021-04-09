(ns shimmers.sketches.sediment
  "Experiment influenced by https://inconvergent.net/2016/shepherding-random-numbers/"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defrecord Particle [pos prev])

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [dx 0.001]
    {:particles (for [i (range 0 1 dx)
                      :let [pos (gv/vec2 (cq/rel-pos i 1.0))]]
                  (Particle. pos pos))}))

(defn velocity [{:keys [pos prev]}]
  (:y (tm/- pos prev)))

(defn update-point [{:keys [pos] :as point} surrounding]
  (let [vel (/ (reduce + (map velocity surrounding))
               (count surrounding))
        acc (* 0.1 (q/random-gaussian))
        vel' (gv/vec2 0 (* 0.999 (+ vel acc)))
        pos' (update (tm/+ pos vel') :y tm/clamp 0 (q/height))]
    (assoc point
           :prev pos
           :pos pos')))

(defn update-particles [particles]
  (concat
   [(update-point (first particles) (take 5 particles))
    (update-point (second particles) (take 5 particles))]
   (for [surrounding (partition 5 1 particles)]
     (update-point (nth surrounding 2) surrounding))
   [(update-point (last (butlast particles)) (drop (- (count particles) 5) particles))
    (update-point (last particles) (drop (- (count particles) 5) particles))]))

(defn update-state [state]
  (update state :particles update-particles))

(defn draw [{:keys [particles]}]
  ;; (q/no-loop)
  (q/no-fill)
  (q/stroke 0 0.05)
  (q/stroke-weight 0.5)
  (doseq [segment (partition 4 1 particles)]
    ;; (apply q/curve (mapcat #(:pos %) segment))
    (doseq [{:keys [pos]} (butlast (drop 1 segment))]
      (q/ellipse (:x pos) (:y pos) 0.2 0.2))
    ))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch sediment
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    ;; :mouse-clicked (fn [state] (q/redraw) state)
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

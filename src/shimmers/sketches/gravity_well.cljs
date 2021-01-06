(ns shimmers.sketches.gravity-well
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]))

(defn make-body []
  (let [initial-pos (v/vec2 (q/random -50 50)
                            (q/random -50 50))]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (v/scale (v/vec2 (q/random-2d)) 0.1)
     :acceleration (v/vec2 0 0)
     :mass (q/random 0.05 0.2)
     :color [0 0 0 96]}))

(defn gravitational-pull
  [{:keys [position mass] :as current} bodies]
  (reduce v/add
          (v/vec2 0 0)
          (for [body bodies
                :when (not= body current)
                :let [gravity 1
                      d2 (tg/dist-squared position (:position body))]]
            (v/scale (tm/normalize (tm/- (:position body) position))
                     (min 0.001 (/ (* gravity (:mass body) mass) d2))))))

(defn update-body
  [bodies {:keys [position velocity acceleration] :as body}]
  (let [new-velocity (v/add velocity acceleration)
        new-position (v/add position new-velocity)]
    (assoc body
           :last-pos position
           :position new-position
           :velocity new-velocity
           :acceleration (gravitational-pull body bodies))))

(defn setup []
  (q/background 255)
  {:bodies (repeatedly 128 make-body)})

(defn update-state [state]
  (update state :bodies (fn [bodies] (map (partial update-body bodies) bodies))))

(defn draw-bodies [bodies]
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [{:keys [position last-pos color mass]} bodies]
    (apply q/stroke color)
    (q/stroke-weight (q/map-range mass 0.05 0.2 0.1 2))
    (q/line last-pos position)))

(defn draw [{:keys [bodies]}]
  (q/background 255 24)
  (draw-bodies bodies))

(defn ^:export run-sketch []
  (q/defsketch gravity-well
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))


(ns shimmers.sketches.gravity-well
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]))

(defrecord Body
    [position last-pos velocity acceleration mass color])

(defn make-body []
  (let [r 150
        initial-pos (v/vec2 (q/random (- r) r)
                            (q/random (- r) r))]
    (map->Body
     {:last-pos initial-pos
      :position initial-pos
      :velocity (v/vec2 0 0)
      :acceleration (v/vec2 0 0)
      :mass (q/random 0.05 0.3)
      :color [0 0 0 96]})))

(defn gravitational-pull
  [{:keys [position mass] :as current} bodies]
  (reduce v/add
          (v/vec2 0 0)
          (for [body bodies
                :when (not= body current)
                :let [gravity 0.2
                      d2 (tg/dist-squared position (:position body))]]
            (v/scale (tm/normalize (tm/- (:position body) position))
                     (/ (* gravity (:mass body) mass)
                        (max d2 1))))))

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
    (q/stroke-weight (q/map-range mass 0.05 0.3 0.3 3))
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


(ns shimmers.sketches.ascendance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defrecord Particle [t0 t1 lifespan decay])

(defn make-particle [t]
  (->Particle t t 100.0 (/ (rand) 10)))

(defn update-particle
  [{:keys [t1 decay] :as p}]
  (assoc p :t0 t1 :t1 (+ t1 decay)))

(defn position [p t h]
  (let [hh (/ h 2)]
    [(* 150 (q/cos t))
     (q/map-range t 0.0 100.0 hh (- hh))
     (* 150 (q/sin t))]))

(defn setup []
  (q/ortho)
  {:particles []})

(defn alive? [{:keys [t1 lifespan]}]
  (< t1 lifespan))

(defn add-particle [particles]
  (let [alive (map update-particle
                   (filter alive? particles))]
    (if (and (< (count alive) 100) (< (rand) 0.01))
      (conj alive (make-particle 0.0))
      alive)))

(defn update-state [state]
  (update state :particles add-particle))

(defn draw [{:keys [particles]}]
  (q/background 255 1)
  (q/color 0)
  (let [h (q/height)]
    (doseq [{:keys [t0 t1] :as p} particles]
      (q/line (position p t0 h) (position p t1 h)))))

(defn ^:export run-sketch []
  (q/defsketch template
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

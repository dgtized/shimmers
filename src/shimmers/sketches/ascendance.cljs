(ns shimmers.sketches.ascendance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defrecord Particle [t0 t1 lifespan decay ascension radius])

(defn make-particle [t]
  (->Particle t t 100.0
              (/ (rand) 2)
              (rand-nth [1.5 2.0 3.0 4.0])
              (rand-nth [(fn [_] 150)
                         (fn [t] (- 150 t))
                         (let [r (rand)]
                           (fn [_] (* r 200)))
                         (fn [t] (/ 150 (+ t 1)))])))

(defn update-particle
  [{:keys [t1 decay] :as p}]
  (assoc p :t0 t1 :t1 (+ t1 decay)))

(defn position [{:keys [ascension radius]} t h]
  (let [hh (/ h 2)
        r (radius t)]
    [(* r (q/cos t))
     (q/map-range (* ascension t) 0.0 100.0 hh (- hh))
     (* r (q/sin t))]))

(defn setup []
  (q/ortho)
  {:particles []})

(defn alive? [{:keys [t1 lifespan]}]
  (< t1 lifespan))

(defn add-particle [particles]
  (let [alive (map update-particle
                   (filter alive? particles))]
    (if (and (< (count alive) 1000)
             (< (rand) 0.05))
      (conj alive (make-particle 0.0))
      alive)))

(defn update-state [state]
  (update state :particles add-particle))

(defn draw [{:keys [particles]}]
  (q/stroke-weight 2)
  (q/stroke 0 64)
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

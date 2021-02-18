(ns shimmers.sketches.ascendance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defrecord Particle [t0 t1 lifespan decay ascension radius weight])

(defn make-particle [t]
  (map->Particle
   {:t0 t :t1 t :lifespan 100.0
    :decay (/ (rand) 16)
    :offset (* Math/PI (rand))
    :ascension (rand-nth [1.5 2.0 3.0 4.0])
    :radius
    (let [r (* 200 (rand))]
      (rand-nth [(fn [_] 150)
                 (fn [t] (- r t))
                 (fn [_] r)
                 (fn [t] (/ r (+ t 1)))]))
    :weight (q/random 1.0 4.0)}))

(defn update-particle
  [{:keys [t1 decay] :as p}]
  (assoc p :t0 t1 :t1 (+ t1 decay)))

(defn position [{:keys [ascension radius offset]} t h]
  (let [hh (/ h 2)
        r (radius t)
        pt (+ (/ (+ offset t) ascension) offset)]
    [(* r (q/cos pt))
     (q/map-range (* ascension t) 0.0 100.0 hh (- hh))
     (* r (q/sin pt))]))

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
  (q/stroke 0 192)
  (let [h (q/height)]
    (doseq [{:keys [t0 t1 weight] :as p} particles]
      (q/stroke-weight weight)
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

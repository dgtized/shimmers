(ns shimmers.sketches.ascendance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defrecord Particle [t0 t1 lifespan delta-v ascension radius mass])

(defn make-particle [t]
  (map->Particle
   (let [delta-v (/ (rand) 16)]
     {:t0 t
      :t1 (+ t (* 20 delta-v))
      :lifespan 100.0
      :delta-v delta-v
      :mass (q/random 1.0 4.0)

      :offset (* Math/PI (rand))
      :ascension (rand-nth [1.5 2.0 3.0 4.0])
      :radius
      (let [r (* 200 (rand))]
        (rand-nth [(fn [_] 150)
                   (fn [t] (- r t))
                   (fn [_] r)
                   (fn [t] (/ r (+ t 1)))]))})))

(defn update-particle
  [{:keys [t0 t1 delta-v] :as p}]
  (assoc p
         :t0 (+ t0 delta-v)
         :t1 (+ t1 delta-v)))

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
    (if (and (< (count alive) 16)
             (< (rand) 0.05))
      (conj alive (make-particle 0.0))
      alive)))

(defn update-state [state]
  (update state :particles add-particle))

(defn draw [{:keys [particles]}]
  (q/background 255)
  (q/stroke 0 192)
  (let [h (q/height)]
    (doseq [{:keys [t0 t1 delta-v mass] :as p} particles
            :let [point-pairs
                  (->> (range t0 t1 (* 4 delta-v))
                       (map (fn [t] (position p t h)))
                       (partition 2 1))]]
      (q/stroke-weight mass)
      (doseq [[p0 p1] point-pairs]
        (q/line p0 p1)))))

(defn ^:export run-sketch []
  (q/defsketch template
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

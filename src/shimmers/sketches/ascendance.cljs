(ns shimmers.sketches.ascendance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defrecord Particle [t0 fuel delta-v velocity radius mass])

(defn make-particle [t0]
  (map->Particle
   {:t0 t0
    :fuel 100.0
    :delta-v (/ (rand) 16)
    :mass (q/random 1.0 4.0)

    :velocity (rand-nth [1.5 2.0 3.0 4.0])
    :radius
    (let [r (* 200 (rand))]
      (rand-nth [(fn [_] 150)
                 (fn [t] (- r t))
                 (fn [_] r)
                 (fn [t] (/ r (+ t 1)))]))}))

(defn update-particle
  [{:keys [delta-v] :as p}]
  (update p :fuel - delta-v))

(defn position [{:keys [t0 velocity radius]} now h]
  (let [hh (/ h 2)
        t (- now t0)
        r (radius t)
        pt (/ t velocity)]
    [(* r (q/cos pt))
     (q/map-range (* velocity t) 0.0 100.0 hh (- hh))
     (* r (q/sin pt))]))

(defn setup []
  (q/ortho)
  {:time 0.0
   :particles []})

(defn alive? [time {:keys [t0 fuel]}]
  (and (> fuel 0.0)
       (< (- time t0) 50.0)))

(defn add-particle [particles t1]
  (let [alive (map update-particle
                   (filter (partial alive? t1) particles))]
    (if (and (< (count alive) 16)
             (< (rand) 0.05))
      (conj alive (make-particle t1))
      alive)))

(def dt 0.1)

(defn update-state [{:keys [time] :as state}]
  (let [t1 (+ time dt)]
    (-> state
        (assoc :time t1)
        (update :particles add-particle t1))))

(defn draw [{:keys [particles time]}]
  (q/background 255)
  (q/stroke 0 192)
  (let [h (q/height)]
    (doseq [{:keys [mass] :as p} particles
            :let [point-pairs
                  (->> (range (- time (* 8 dt)) time (* 2 dt))
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

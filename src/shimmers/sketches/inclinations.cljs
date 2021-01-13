(ns shimmers.sketches.inclinations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defrecord Particle [source last-pos position velocity acceleration])

(defn in-bounds? [{:keys [position]}]
  (let [[x y] position
        r 200]
    (< (+ (* x x) (* y y)) (* r r))))

(defn alive? [{:keys [lifespan]}]
  (> lifespan 0))

(defn make-particle [source position velocity]
  (map->Particle
   {:source source
    :last-pos position
    :position position
    :velocity velocity
    :acceleration (v/vec2 0 0)
    :color [0 0 0 96]
    :lifespan (rand-int 500)}))

(defn create-emitter [position n]
  {:position position
   :max-particles n
   :probability 0.1})

(defn setup []
  (let [size 50]
    {:emitters [(create-emitter (v/vec2 size size) 64)
                (create-emitter (v/vec2 size (- size)) 64)
                (create-emitter (v/vec2 (- size) size) 64)
                (create-emitter (v/vec2 (- size) (- size)) 64)]
     :particles []}))

(defn update-particle
  [{:keys [position velocity acceleration] :as particle}]
  (let [new-velocity (v/add velocity acceleration)]
    (assoc (update particle :lifespan dec)
           :last-pos position
           :position (v/add position new-velocity)
           :velocity new-velocity
           :acceleration (v/scale (v/vec2 (q/random-2d)) 0.05))))

(defn update-state [{:keys [particles emitters] :as state}]
  (let [active-particles (filterv (every-pred in-bounds? alive?) particles)
        particles-by-source (map-kv count (group-by :source active-particles))
        emissions (for [{:keys [probability position max-particles] :as emitter} emitters
                        :when (and (< (get particles-by-source emitter 0) max-particles)
                                   (< probability (rand)))]
                    (make-particle emitter position (v/scale (v/vec2 (q/random-2d)) 0.01)))]
    (assoc state :particles (map update-particle (concat active-particles emissions)))))

(defn draw-particles [particles]
  (doseq [{:keys [position last-pos color]} particles]
    (apply q/stroke color)
    (let [[lx ly] last-pos
          [x y] position]
      (q/stroke-weight (q/random 0.3 1.0))
      (q/line lx ly x y))))

(defn draw [{:keys [particles]}]
  (q/background 255 8)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-particles particles))

(defn ^:export run-sketch []
  (q/defsketch inclinations
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

(ns shimmers.sketches.emitters
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.particle-system :as particles]
            [shimmers.common.sequence :refer [map-kv]]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]))

(defrecord Particle [source last-pos position velocity acceleration color lifespan])

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
    :color [0 0 0 128]
    :lifespan (rand-int 1000)}))

(defn create-emitter [position n]
  {:position position
   :max-particles n
   :probability 0.9})

(defn setup []
  (let [size 50
        n 256]
    {:emitters [(create-emitter (v/vec2 size size) n)
                (create-emitter (v/vec2 size (- size)) n)
                (create-emitter (v/vec2 (- size) size) n)
                (create-emitter (v/vec2 (- size) (- size)) n)]
     :particles []}))

(defn update-particle
  [particle]
  (-> particle
      (update :lifespan dec)
      (assoc :acceleration (v/scale (v/vec2 (q/random-2d)) 0.01))
      particles/step))

(defn update-state [{:keys [particles emitters] :as state}]
  (let [active-particles (filterv (every-pred in-bounds? alive?) particles)
        particles-by-source (map-kv count (group-by :source active-particles))
        emissions (for [{:keys [probability position max-particles] :as emitter} emitters
                        :when (and (< (get particles-by-source emitter 0) max-particles)
                                   (p/chance probability))]
                    (make-particle emitter position (v/scale (v/vec2 (q/random-2d)) 0.001)))]
    (assoc state :particles (map update-particle (concat active-particles emissions)))))

(defn draw [{:keys [particles]}]
  (q/background 255 8)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (particles/draw particles :weight #(q/random 0.3 1.0)))

(defn ^:export run-sketch []
  (q/defsketch emitters
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

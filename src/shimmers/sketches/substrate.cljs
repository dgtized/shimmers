(ns shimmers.sketches.substrate
  "Inspired by http://www.complexification.net/gallery/machines/substrate/ and using https://discourse.processing.org/t/trying-to-understand-the-substrate-algorithm/3031/14 for guidance."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn in-bounds? [[x y]]
  (and (>= x 0) (< x (q/width))
       (>= y 0) (< y (q/height))))

(defrecord Crack [start position angle parent active])

(defn make-crack [position angle & parent]
  (->Crack position position angle parent true))

(defn spawn-crack [{:keys [position angle] :as crack}]
  (make-crack position
              (+ angle
                 (* (rand-nth [-1 1]) (/ Math/PI 2))
                 (* 0.15 (q/random-gaussian)))
              crack))

(defn intersects [self point crack]
  (cond (= self crack)
        false
        (= (:parent self) crack)
        false
        :else (let [{:keys [start position]} crack
                    dseg (v/distance start position)
                    dstart (v/distance start point)
                    dend (v/distance point position)]
                (< (Math/abs (- dseg dstart dend)) 0.005))))

(defn update-crack [cracks {:keys [position angle] :as crack}]
  (let [new-pos (v/add position (v/scale (v/unit2-from-angle angle) 0.8))]
    (if (or (not (in-bounds? position))
            (some (partial intersects crack new-pos) cracks))
      (update crack :active not)
      (assoc crack :position new-pos))))

(defn make-random-crack []
  (make-crack (v/vec2 (q/random (q/width)) (q/random (q/height)))
              (* (rand-nth [0 1 2 3]) (/ Math/PI 2))))

(defn setup []
  {:cracks (repeatedly 8 make-random-crack)})

(defn update-cracks [cracks]
  (let [by-active (group-by :active cracks)
        active (get by-active true)
        inactive (get by-active false)
        fresh-cracks (if (and (< (rand) 0.15) (not-empty active))
                       (conj active (spawn-crack (rand-nth active)))
                       active)]
    (concat inactive
            (map (partial update-crack cracks)
                 fresh-cracks))))

(defn update-state [state]
  (update state :cracks update-cracks))

(defn draw [{:keys [cracks]}]
  (q/background 255 32)
  (q/stroke-weight 0.6)
  (q/stroke 0 64)
  (doseq [{:keys [start position]} cracks]
    (q/line start position)))

(defn ^:export run-sketch []
  (q/defsketch substrate
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

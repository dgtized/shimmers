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
                 (* 0.1 (q/random-gaussian)))
              crack))

(defn intersects [self point crack]
  (cond (= self crack)
        false
        (= (:parent self) crack)
        false
        :else false))

(defn update-crack [cracks {:keys [position angle parent] :as crack}]
  (let [new-pos (v/add position (v/scale (v/unit2-from-angle angle) 0.3))]
    (if (or (not (in-bounds? position))
            (some (partial intersects crack new-pos) cracks))
      (update crack :active not)
      (assoc crack :position new-pos))))

(defn setup []
  {:cracks [(make-crack (v/vec2 0 (/ (q/height) 2)) 0)
            (make-crack (v/vec2 (/ (q/width) 2) 0) (/ Math/PI 2))]})

(defn update-cracks [cracks]
  (let [fresh-cracks (if (< (rand) 0.03)
                       (conj cracks (spawn-crack (rand-nth cracks)))
                       cracks)]
    (concat
     (remove :active fresh-cracks)
     (map (partial update-crack cracks)
          (filter :active fresh-cracks)))))

(defn update-state [state]
  (update state :cracks update-cracks))

(defn draw [{:keys [cracks]}]
  (q/background 255)
  (q/stroke-weight 0.8)
  (q/stroke 0 96)
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

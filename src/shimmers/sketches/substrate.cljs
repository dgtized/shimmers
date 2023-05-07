(ns shimmers.sketches.substrate
  "Inspired by http://www.complexification.net/gallery/machines/substrate/ and
  using
  https://discourse.processing.org/t/trying-to-understand-the-substrate-algorithm/3031/14
  for guidance."
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

(defn in-bounds? [[x y]]
  (and (>= x 0) (< x (q/width))
       (>= y 0) (< y (q/height))))

(defrecord Crack [start position angle parent active])

(defn make-crack [position angle & parent]
  (->Crack position position angle parent true))

(defn spawn-crack [{:keys [position angle] :as crack}]
  (make-crack position
              (+ angle
                 (* (dr/rand-nth [-1 1]) (/ Math/PI 2))
                 (* 0.15 (dr/gaussian)))
              crack))

(defn intersects [self point crack]
  (if (#{self (:parent self)} crack)
    false
    (let [{:keys [start position]} crack]
      (poly-detect/point-on-line? start position point 0.005))))

(defn update-crack [cracks {:keys [position angle] :as crack}]
  (let [new-pos (v/add position (v/polar 0.33 angle))]
    (if (or (not (in-bounds? position))
            (some (partial intersects crack new-pos) cracks))
      (update crack :active not)
      (assoc crack :position new-pos))))

(defn make-random-crack []
  (make-crack (cq/rel-vec (dr/random) (dr/random))
              (* (dr/rand-nth [0 1 2 3]) (/ Math/PI 2))))

(defn create-cracks []
  {:cracks (repeatedly 16 make-random-crack)})

(defn setup []
  (create-cracks))

(defn update-cracks [{:keys [cracks] :as state}]
  (let [[active inactive] ((juxt filter remove) :active cracks)
        fresh-cracks (into active (if (< (count active) 64)
                                    (for [crack active
                                          :when (dr/chance 0.01)]
                                      (spawn-crack crack))
                                    []))]
    ;; (println [(count inactive) (count active)])
    [(empty? fresh-cracks)
     (->> fresh-cracks
          (map (partial update-crack cracks))
          (concat inactive)
          (assoc state :cracks))]))

(defn update-state [state]
  (cq/if-steady-state state 5
                      create-cracks
                      update-cracks))

(defn draw [{:keys [cracks]}]
  (q/background 255 32)
  (q/stroke-weight 0.6)
  (q/stroke 0 64)
  (doseq [{:keys [start position]} cracks]
    (q/line start position)))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition substrate
  {:created-at "2021-01-21"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))

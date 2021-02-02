(ns shimmers.sketches.bubbles
  (:require [cljs.core.match :refer-macros [match]]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn make-bubble []
  {:position (v/vec2 (q/random (q/width)) (q/height))
   :size (+ 1 (rand-int 6))})

(defn setup []
  {:bubbles []})

(defn in-bounds? [[_ y]]
  (<= 0 y (q/height)))

(defn update-bubble [{:keys [position size] :as bubble}]
  (when (in-bounds? position)
    (assoc bubble :position (v/sub position (v/vec2 0 (/ 0.3 size))))))

(defn combine-bubble [a b]
  (let [arad (:size a)
        brad (:size b)]
    {:position (if (> arad brad)
                 (:position a)
                 (:position b))
     :size (Math/sqrt (+ (Math/pow arad 2)
                         (Math/pow brad 2)))}))

(defn intersects? [a b]
  (when (< (apply v/distance (map :position [a b]))
           (+ (:size a) (:size b)))
    b))

(defn combine-intersecting [bubbles]
  (loop [ordered (sort-by (comp :position :x) bubbles)
         results []]
    (match [ordered]
      [([] :seq)] results
      [([a] :seq)] (conj results a)
      [([a & xs] :seq)]
      (if-let [b (some (partial intersects? a) xs)]
        (recur (conj (remove #{b} xs) (combine-bubble a b)) results)
        (recur xs (conj results a))))))

(defn update-state [{:keys [bubbles] :as state}]
  (let [active (keep update-bubble (combine-intersecting bubbles))]
    (assoc state :bubbles
           (if (and (< (rand) 0.04)
                    (< (count active) 512))
             (conj active (make-bubble))
             active))))

(defn draw [{:keys [bubbles]}]
  (q/background 250 150 140 32)
  (q/no-fill)
  (q/stroke 0 192)
  (doseq [{:keys [position size]} bubbles
          :let [[x y] position]]
    (q/ellipse x y size size)))

(defn ^:export run-sketch []
  (q/defsketch bubbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

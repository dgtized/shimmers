(ns shimmers.sketches.bubbles
  (:require [cljs.core.match :refer-macros [match]]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn make-bubble []
  {:position (v/vec2 (q/random (q/width)) (q/height))
   :size 10})

(defn setup []
  {:bubbles []})

(defn in-bounds? [[_ y]]
  (<= 0 y (q/height)))

(defn update-bubble [{:keys [position] :as bubble}]
  (when (in-bounds? position)
    (assoc bubble :position (v/sub position (v/vec2 0 0.1)))))

(defn combine-bubble [a b]
  {:position (:position a) ;; TODO weighted average position?
   :size (+ (:size a) (:size b))})

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
           (if (and (< (rand) 0.01)
                    (< (count active) 512))
             (conj active (make-bubble))
             active))))

(defn draw [{:keys [bubbles]}]
  (q/background 250 150 140 32)
  (q/no-fill)
  (q/stroke 0 192)
  (doseq [{:keys [position size]} bubbles]
    (apply q/ellipse (concat position [size]))))

(defn ^:export run-sketch []
  (q/defsketch bubbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

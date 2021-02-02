(ns shimmers.sketches.bubbles
  (:require [cljs.core.match :refer-macros [match]]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.circle :as tc]
            [thi.ng.geom.core :as geom]))

(defn make-bubble []
  (tc/circle (v/vec2 (q/random (q/width)) (q/height))
             (+ 1 (rand-int 6))))

(defn setup []
  {:bubbles []})

(defn in-bounds? [c]
  (<= 0 (get-in c [:p :y]) (q/height)))

(defn update-bubble [bubble]
  (when (in-bounds? bubble)
    (geom/translate bubble (v/vec2 0 (- (/ 0.3 (:r bubble)))))))

(defn combine-bubble [a b]
  (tc/circle (if (> (:r a) (:r b))
               (:p a)
               (:p b))
             (Math/sqrt (+ (Math/pow (:r a) 2)
                           (Math/pow (:r b) 2)))))

(defn combine-intersecting [bubbles]
  (loop [ordered (sort-by (comp :p :x) bubbles)
         results []]
    (match [ordered]
      [([] :seq)] results
      [([a] :seq)] (conj results a)
      [([a & xs] :seq)]
      (if-let [b (some (fn [b] (when (geom/intersect-shape a b) b)) xs)]
        (recur (conj (remove #{b} xs) (combine-bubble a b)) results)
        (recur xs (conj results a))))))

(defn update-state [{:keys [bubbles] :as state}]
  (let [active (keep update-bubble (combine-intersecting bubbles))]
    (assoc state :bubbles
           (if (and (< (rand) 0.03)
                    (< (count active) 512))
             (conj active (make-bubble))
             active))))

(defn draw [{:keys [bubbles]}]
  (q/background 250 150 140 32)
  (q/no-fill)
  (q/stroke 0 192)
  (doseq [{:keys [p r]} bubbles
          :let [[x y] p]]
    (q/ellipse x y r r)))

(defn ^:export run-sketch []
  (q/defsketch bubbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

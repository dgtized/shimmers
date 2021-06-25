(ns shimmers.sketches.bubbles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as tc]
            [thi.ng.geom.core :as geom]))

(defn make-bubble []
  (tc/circle (cq/rel-pos (rand) 1.0)
             (+ 1 (rand-int 4))))

(defn setup []
  {:bubbles []})

(defn in-bounds? [c]
  (<= 0 (get-in c [:p :y]) (q/height)))

(defn update-bubble [bubble]
  ;; consider adding acc/vel and horizontal motion from wind?
  (when (in-bounds? bubble)
    (geom/translate bubble (v/vec2 0 (- (/ 0.25 (:r bubble)))))))

(defn combine-bubble [a b]
  (tc/circle (if (> (:r a) (:r b))
               (:p a)
               (:p b))
             (Math/sqrt (+ (Math/pow (:r a) 2)
                           (Math/pow (:r b) 2)))))

(defn combine-intersecting [bubbles]
  (loop [ordered (sort-by (comp :p :x) bubbles)
         results []]
    (if (empty? ordered)
      results
      (let [[a & xs] ordered
            b (some (fn [b] (when (geom/intersect-shape a b) b)) xs)]
        (if b
          (recur (conj (remove #{b} xs) (combine-bubble a b)) results)
          (recur xs (conj results a)))))))

(defn update-state [{:keys [bubbles] :as state}]
  (let [active (keep update-bubble (combine-intersecting bubbles))]
    (assoc state :bubbles
           (if (and (p/chance 0.03)
                    (< (count active) 512))
             (conj active (make-bubble))
             active))))

(defn draw [{:keys [bubbles]}]
  ;; color cycle for streaking effect
  ;; TODO: consider just cycling across predefined color curve?
  (let [fc (q/frame-count)]
    (when (zero? (mod fc 4))
      (q/background (+ 150 (* 100 (q/noise 0 (/ fc 360))))
                    (+ 150 (* 100 (q/noise 64 (/ fc 540))))
                    (+ 150 (* 100 (q/noise 128 (/ fc 720))))
                    8)))
  (q/no-fill)
  (q/stroke-weight 0.05)
  (q/stroke 40 40 240 96)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r]} bubbles]
    (cq/circle p r)))

(sketch/defquil bubbles
  :created-at "2021-02-02"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

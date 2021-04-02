(ns shimmers.sketches.point-to-point
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Some experiments with drawing lines derived from a set of random points

(defn short-pairs [points]
  (->> (for [[u v] (cs/all-pairs points)]
         [(geom/dist u v) [u v]])
       (sort-by first)
       (map second)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles
   (map #(assoc {}
                :p %
                :theta (- (* 2 Math/PI (rand)) Math/PI)
                :radius (tm/clamp (+ 0.05 (* 0.02 (q/random-gaussian))) 0 0.5))
        (geometry/generate-points 24 #(+ 0.5 (* 0.13 (q/random-gaussian)))))})

(defn sign+
  "Increase magnitude of `n` by `v` without changing sign of `n`"
  [n v]
  ((if (> n 0) + -) n v))

(defn update-state [{:keys [circles] :as state}]
  (assoc state
         :circles (map #(update % :theta sign+ 0.01) circles)
         :points
         (for [{:keys [p theta radius]} circles]
           (geom/translate p (geom/as-cartesian (gv/vec2 radius theta))))))

(defn all-lines [points]
  (doseq [point points
          :let [[x y] (cq/rel-pos point)]]
    (q/line 0 y (q/width) y)
    (q/line x 0 x (q/height))))

(defn shortest-to-edge [points]
  (for [[x y] points]
    [[x y]
     (cond (and (>= x 0.5) (>= y 0.5))
           (if (> x y)
             [1.0 y]
             [x 1.0])
           (and (< x 0.5) (< y 0.5))
           (if (> x y)
             [x 0.0]
             [0.0 y])
           (and (>= x 0.5) (< y 0.5))
           (if (> (- 1.0 x) y)
             [x 0.0]
             [1.0 y])
           (and (< x 0.5) (>= y 0.5))
           (if (> x (- 1.0 y))
             [x 1.0]
             [0.0 y])
           )]))

(defn draw [{:keys [points]}]
  (q/background 1.0 1.0)
  (q/stroke-weight 2)
  (q/ellipse-mode :radius)
  (q/stroke 0 0 0)
  (doseq [point points
          :let [[x y] (cq/rel-pos point)]]
    (q/ellipse x y 0.2 0.2))

  (q/stroke-weight 0.5)
  (q/stroke 0.99 0.5 0.5)
  (doseq [[p q] (take (* 1.2 (count points)) (short-pairs points))]
    (q/line (cq/rel-pos p) (cq/rel-pos q)))

  (q/stroke 0 0 0)
  (q/stroke-weight 0.2)
  (all-lines points)

  (q/stroke 0.45 0.3 0.5)
  (q/stroke-weight 1.0)
  (doseq [[p q] (shortest-to-edge points)]
    (q/line (cq/rel-pos p) (cq/rel-pos q)))
  )

(defn ^:export run-sketch []
  (q/defsketch point-to-point
    :host "quil-host"
    :size [600 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

(ns shimmers.sketches.iterative-displacement
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.utils.subdiv :as gsd]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Concept here is to displace segments on a line, but ensure they don't clip
;; the line before or after. Trying to find something that generates something
;; like wood-grain while ensuring no overlap between lines. This is too squiggly
;; for now, but worth generalizing and exploring.

;; Consider adding "eyes" ala knots in wood. Displacing points based on a global
;; bias ala flow fields from noise might add some interesting results here.

(defn vline [x]
  [(gv/vec2 x 0.1) (gv/vec2 x 0.9)])

(defn init-lines [n]
  (map vline (tm/norm-range (inc n))))

(defn weighted-point
  [points]
  (let [segments (partition 2 1 points)
        weights (cs/mapping (fn [[p q]] (geom/dist p q)) segments)
        sample (tm/random (apply + (vals weights)))]
    (loop [cumulative 0.0
           i 0
           [[choice weight] & remaining] weights]
      (when weight
        (let [sum (+ cumulative weight)]
          (if (< sample sum)
            [choice (/ (- sample cumulative) weight) i]
            (recur sum (inc i) remaining)))))))

(defn closest-segment [barrier point]
  (apply min-key (fn [p] (geom/dist p point))
         (map (fn [[p q]] (geom/closest-point (gl/line2 p q) point))
              (partition 2 1 barrier))))

;; keeps the first and last point anchored, but smooths in-between
(defn smooth-line [points]
  (if (> (count points) 16)
    (->> points
         (gsd/subdivide-closed (:chaikin gsd/schemes))
         cs/midsection
         (cs/sandwich points))
    points))

(defn simplify-line [points tolerance]
  (->> points
       (partition 3 1)
       (keep (fn [[a b c]]
               (let [ab (geom/heading (tm/- b a))
                     bc (geom/heading (tm/- c b))]
                 (when (> (Math/abs (- ab bc)) tolerance)
                   b))))
       (cs/sandwich points)))

;; Needs to look in a larger window, triangle inequality forces things here
(defn remove-bumps [points margin]
  (->> (partition 3 1 points)
       (keep (fn [[a b c]]
               (when (> (+ (geom/dist a b) (geom/dist b c))
                        (* (geom/dist a c) margin))
                 b)))
       (cs/sandwich points)))

(defn displace-line [line lower upper]
  (let [[[p q] weight i] (weighted-point line)
        [before after] (split-at (inc i) line)
        point (tm/mix p q weight)
        low (closest-segment lower point)
        high (closest-segment upper point)]
    (concat before [(tm/mix (tm/mix low high (tm/random 0.2 0.8))
                            (tm/mix p q (tm/random 0.4 0.6))
                            (tm/random 0.1 0.3))] after)))

(defn update-random-line
  [lines]
  (let [groups (partition 3 1 lines)
        k (rand-int (count groups))]
    (->> groups
         (map-indexed
          (fn [idx [lower line upper]]
            (if (and (= idx k) (< (count line) 128))
              (-> line
                  (displace-line lower upper)
                  smooth-line
                  ;; (remove-bumps 0.9)
                  (simplify-line 0.01))
              line)))
         (cs/sandwich lines))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines (init-lines 48)})

(defn update-state [state]
  (update state :lines update-random-line))

(defn draw [{:keys [lines]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [points (butlast (rest lines))]
    (q/begin-shape)
    (doseq [[x y] (map cq/rel-pos points)]
      (q/vertex x y))
    (q/end-shape)))

(sketch/defquil iterative-displacement
  :created-at "2021-08-22"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

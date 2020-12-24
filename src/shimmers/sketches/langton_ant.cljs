(ns shimmers.sketches.langton-ant
  "References:
   * https://en.wikipedia.org/wiki/Langton%27s_ant
   * https://thecodingtrain.com/CodingChallenges/089-langtonsant.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn turn-right [dir] (+ dir (/ Math/PI 2)))
(defn turn-left [dir] (- dir (/ Math/PI 2)))

(defn create-grid []
  {(v/vec2 0 0) false})

(defn create-ant [position direction]
  {:position position :direction direction})

(defn setup []
  {:grid (create-grid)
   :ants [(create-ant (v/vec2 0 0) 0)]})

(defn advance [grid ant]
  (let [new-pos (v/add (:position ant) (v/unit2-from-angle (:direction ant)))]
    [(update grid new-pos (fnil identity false))
     (assoc ant :position new-pos)]))

(defn move-ant [grid ant]
  (let [pixel (get grid (:position ant))]
    (advance (update grid (:position ant) not)
             (if pixel
               (update ant :direction turn-right)
               (update ant :direction turn-left)))))

;; TODO: make this work for N ants
(defn update-state [{:keys [grid ants]}]
  (let [[new-grid new-ant] (move-ant grid (first ants))]
    {:grid new-grid :ants [new-ant]}))

(defn grid-range [grid f]
  (let [xs (map f (keys grid))]
    [(reduce min xs) (reduce max xs)]))

(defn draw [{:keys [grid ants] :as state}]
  (q/background 255)
  (q/fill 0)
  (q/rect-mode :center)
  (let [[x0 x1] (grid-range grid first)
        [y0 y1] (grid-range grid second)
        center (v/vec2 (/ (q/width) 2) (/ (q/height) 2))
        r (/ (+ (q/width) (q/height))
             (* 3 (+ (- x1 x0) (- y1 y0))))]
    (doseq [[position value] grid]
      (when value
        (let [[x y] (v/add center (v/scale position r))]
          (q/rect x y r r))))))

(defn ^:export run-sketch []
  (q/defsketch langton-ant
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [framerate/mode m/fun-mode]))

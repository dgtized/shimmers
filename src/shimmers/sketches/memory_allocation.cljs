(ns shimmers.sketches.memory-allocation
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.automata.memory :as mem]
   [shimmers.common.framerate :as framerate]
   [shimmers.math.probability :as p]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [pages (Math/pow 2 12)]
    (mem/initialize pages)))

(defn update-state [{:keys [allocations] :as state}]
  (cond (p/chance 0.5)
        state
        (and (not-empty allocations) (p/chance 0.3))
        (mem/free state (rand-nth (dedupe (sort (map :id allocations)))))
        :else
        (mem/malloc state (int (tm/random 4 128)))))

(def phi (/ (+ 1 (Math/sqrt 5)) 2))
(defn color [id]
  [(mod (* id phi) 1.0) 0.5 0.5 1.0])

(defn draw [{:keys [pages allocations]}]
  (q/background 1.0)
  (q/no-fill)
  (let [aspect (/ (q/width) (q/height))
        cols (int (Math/sqrt pages))
        w (/ (/ (q/width) cols) aspect)
        h (/ (q/height) (/ pages cols))]
    (doseq [y (range 0 (/ pages cols))]
      (doseq [x (range 0 cols 1)]
        (q/rect (* x w) (* y h) w h)))

    (doseq [{:keys [id base size]} allocations]
      (q/fill (color id))
      (dotimes [i size]
        (let [offset (+ base i)
              x (rem offset cols)
              y (quot offset cols)]
          (q/rect (* x w) (* y h) w h))))))

(sketch/defquil memory-allocation
  :created-at "2021-10-14"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

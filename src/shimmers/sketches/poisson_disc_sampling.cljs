(ns shimmers.sketches.poisson-disc-sampling
  "https://www.youtube.com/watch?v=flQgnCUxHlw"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

(defn poisson-disc-init [bounds r k n]
  (let [w (/ r (Math/sqrt 2))
        p (geom/random-point-inside bounds)
        [x y] p
        row (Math/floor (/ x w))
        col (Math/floor (/ y w))]
    {:bounds bounds
     :r r
     :k k
     :n n
     :w w
     :grid {[row col] p}
     :active [p]}))

(defn neighbors [grid row col]
  (for [i [-1 0 1]
        j [-1 0 1]
        :let [neighbor (get grid [(+ row i) (+ col j)])]
        :when neighbor]
    neighbor))

(defn maybe-add-sample [considering {:keys [r w grid active bounds] :as state}]
  (let [sample (v/add considering
                      (v/polar (tm/random r (* 2 r))
                               (tm/random tm/TWO_PI)))
        [sx sy] sample
        row (Math/floor (/ sx w))
        col (Math/floor (/ sy w))]
    (if (and (geom/contains-point? bounds sample)
             (every? (fn [neighbor] (>= (geom/dist sample neighbor) r))
                     (neighbors grid row col)))
      (assoc state
             :active (conj active sample)
             :grid (assoc grid [row col] sample))
      state)))

(defn iterate-cycles
  "Iterate on `x` using `f` for `n` cycles, returning the final `x`."
  [n f x]
  (last (take (inc n) (iterate f x))))

(defn poisson-disc-fill [{:keys [n] :as state0}]
  (iterate-cycles n
                  (fn [{:keys [k active] :as state}]
                    (if (> (count active) 0)
                      (let [considering (rand-nth active)
                            state' (iterate-cycles k (partial maybe-add-sample considering) state)]
                        (if (= state state')
                          (update state' :active (partial remove #(= considering %)))
                          state'))
                      state))
                  state0))


(defn setup []
  (q/color-mode :hsl 1.0)
  (poisson-disc-init (rect/rect (cq/rel-pos 0.1 0.1) (cq/rel-pos 0.9 0.9))
                     8 30 30))

(defn update-state [state]
  (poisson-disc-fill state))

(defn draw [{:keys [grid]}]
  (q/background 255)
  (q/stroke 0)
  (q/stroke-weight 2)
  (doseq [[x y] (vals grid)]
    (q/point x y)))

(sketch/defquil poisson-disc-sampling
  :created-at "2021-06-30"
  :size [600 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

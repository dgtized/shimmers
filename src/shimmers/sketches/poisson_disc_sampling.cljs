(ns shimmers.sketches.poisson-disc-sampling
  "https://www.youtube.com/watch?v=flQgnCUxHlw"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as tm]))

;; https://www.cs.ubc.ca/~rbridson/docs/bridson-siggraph07-poissondisk.pdf
;; http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
;; https://arxiv.org/abs/2004.06789 -- Fast Variable Density Poisson-Disc Sample Generation with Directional Variation
(defn poisson-disc-init [bounds r k n]
  (let [w (/ r (Math/sqrt 2))
        p (g/random-point-inside bounds)
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
    (if (and (g/contains-point? bounds sample)
             (every? (fn [neighbor] (>= (g/dist sample neighbor) r))
                     (neighbors grid row col)))
      (assoc state
             :active (conj active sample)
             :grid (assoc grid [row col] sample))
      state)))

(defn poisson-disc-fill [{:keys [k active] :as state}]
  (if (> (count active) 0)
    (let [considering (rand-nth active)
          state' (cs/iterate-cycles k (partial maybe-add-sample considering) state)]
      (if (= state state')
        (update state' :active (partial remove #(= considering %)))
        state'))
    state))

(def ui-state
  (ctrl/state {:radius 8 :samples 10}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [radius samples]} @ui-state]
    (poisson-disc-init (cq/screen-rect 0.8) radius samples 10)))

(defn update-state [{:keys [n] :as state}]
  (cs/iterate-cycles n poisson-disc-fill state))

(defn draw [{:keys [active grid]}]
  (q/background 255)
  (q/stroke 0)
  (q/stroke-weight 2)
  (doseq [[x y] (vals grid)]
    (q/point x y))
  (q/stroke-weight 4)
  (q/stroke 0 0.5 0.5)
  (doseq [[x y] active]
    (q/point x y)))

(defn ui-controls []
  (ctrl/container
   (ctrl/slider ui-state (fn [v] (str "Min Separation: " v))
                [:radius] [2 16 1])
   ;; Is this parameter even worth tuning?
   (ctrl/slider ui-state (fn [v] (str "Samples per Location: " v))
                [:samples] [5 50 1])))

(sketch/defquil poisson-disc-sampling
  :created-at "2021-06-30"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

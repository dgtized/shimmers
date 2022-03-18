(ns shimmers.algorithm.poisson-disc-sampling
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn grid-location [w [x y]]
  [(Math/floor (/ x w))
   (Math/floor (/ y w))])

;; Various discussion on variable density sampling:
;; https://www.cs.ubc.ca/~rbridson/docs/bridson-siggraph07-poissondisk.pdf
;; http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
;; https://arxiv.org/abs/2004.06789 -- Fast Variable Density Poisson-Disc Sample Generation with Directional Variation
(defn init [bounds r k n]
  (let [dims 2
        w (/ r (Math/sqrt dims))
        p (g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))]
    {:bounds bounds
     :r r
     :k k
     :n n;; => nil
     :w w
     :grid {(grid-location w p) p}
     :active [p]}))

(defn neighbors
  "Find all points in the `grid` in the surrounding neighborhood of radius `size`."
  [grid size [row col]]
  (let [surroundings (range (- size) (inc size) 1)]
    (for [i surroundings
          j surroundings
          :let [neighbor (get grid [(+ row i) (+ col j)])]
          :when neighbor]
      neighbor)))

(defn maybe-add-sample [considering {:keys [r w grid active bounds] :as state}]
  (let [sample (v/add considering
                      (v/polar (dr/random r (* 2 r))
                               (dr/random tm/TWO_PI)))
        location (grid-location w sample)]
    (if (and (g/contains-point? bounds sample)
             (every? (fn [neighbor] (>= (g/dist sample neighbor) r))
                     (neighbors grid 1 location)))
      (assoc state
             :active (conj active sample)
             :grid (assoc grid location sample))
      state)))

(defn fill-step [{:keys [k active] :as state}]
  (if (empty? active)
    state
    (let [considering (dr/rand-nth active)
          state' (cs/iterate-cycles k (partial maybe-add-sample considering) state)]
      (if (= state state')
        (update state' :active (partial remove #(= considering %)))
        state'))))

(defn generate [bounds radius k-attempts n]
  (let [{:keys [grid]}
        (->> (init bounds radius k-attempts n)
             (iterate fill-step)
             (take-while (fn [{:keys [active]}] (not-empty active)))
             last)]
    (vals grid)))

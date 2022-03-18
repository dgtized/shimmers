(ns shimmers.algorithm.poisson-disc-sampling
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Various discussion on variable density sampling:
;; https://www.cs.ubc.ca/~rbridson/docs/bridson-siggraph07-poissondisk.pdf
;; http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
;; https://arxiv.org/abs/2004.06789 -- Fast Variable Density Poisson-Disc Sample Generation with Directional Variation
(defn init [bounds r k n]
  (let [w (/ r (Math/sqrt 2))
        p (g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))
        [x y] p
        row (Math/floor (/ x w))
        col (Math/floor (/ y w))]
    {:bounds bounds
     :r r
     :k k
     :n n;; => nil
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
                      (v/polar (dr/random r (* 2 r))
                               (dr/random tm/TWO_PI)))
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

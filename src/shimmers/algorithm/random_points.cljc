(ns shimmers.algorithm.random-points
  (:require
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn cell-fit [{[w h] :size} n]
  (let [ratio (if (> h w) (/ w h) (/ h w))
        r (Math/ceil (* ratio (Math/sqrt n)))
        c (Math/ceil (/ n r))]
    {:rows r :cols c}))

(comment (cell-fit {:size [80 60]} 100)
         (cell-fit {:size [60 80]} 100))

;; https://stats.stackexchange.com/questions/481543/generating-random-points-uniformly-on-a-disk
(defn inside-circle
  ([c] (inside-circle c tm/random))
  ([{:keys [p r]} random]
   (-> (gv/vec2
        (* r (Math/sqrt (random)))
        (* tm/TWO_PI (random)))
       g/as-cartesian
       (tm/+ p))))

(defn random-points
  "Generate `n` random points within a given `bounds`."
  [bounds n]
  (repeatedly n #(g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))))

;; Generates *close* to n points
(defn random-cells
  "Subdivide into ~`n` cells and pick a random point in each cell."
  [bounds n]
  (let [cells (g/subdivide bounds (cell-fit bounds n))]
    (for [cell cells]
      (g/unmap-point cell (gv/vec2 (dr/random) (dr/random))))))

;; Generates *close* to n points
(defn random-cell-jitter
  "Subdivide into ~`n` cells and then create a circle just touching the inside of
  the cell and pick a random point inside that circle."
  [bounds n]
  (let [cells (g/subdivide bounds (cell-fit bounds n))]
    (for [{[w h] :size :as cell} cells]
      (inside-circle (gc/circle (g/centroid cell) (/ (min w h) 2))
                     dr/random))))

(defn poisson-disc-sampling
  "Generate ~`n` random points in a boundary using poisson disc sampling.

  Note the automatic radius use of PHI is just a magic constant that just seems
  to work. Usually results in a few more points than requested given the radius."
  [bounds n]
  (let [radius (/ (Math/sqrt (g/area bounds)) (Math/sqrt (* tm/PHI n)))]
    (pds/generate bounds 12 radius)))

(defn sample-pool-replacement
  "Given a seq of points, return a function that will sample each of those points
  in a random order. Once the pool is emptied, it's reshuffled and sampled all
  over again."
  [points]
  (let [pool (atom (dr/shuffle points))]
    (fn [] (let [[v & r] @pool]
            (reset! pool (if (seq r) r (dr/shuffle points)))
            v))))

(def modes
  {:random-points random-points
   :random-cells random-cells
   :random-cell-jitter random-cell-jitter
   :poisson-disc-sampling poisson-disc-sampling} )

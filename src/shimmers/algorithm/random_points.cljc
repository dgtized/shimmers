(ns shimmers.algorithm.random-points
  (:require
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Circle2 Polygon2 Rect2 Triangle2]])
   [thi.ng.math.core :as tm])
  #?(:clj (:import [thi.ng.geom.types Circle2 Polygon2 Rect2 Triangle2])))

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

;; https://observablehq.com/@jrus/halton
;; https://pbr-book.org/3ed-2018/Sampling_and_Reconstruction/The_Halton_Sampler
;; https://www.mathworks.com/help/stats/haltonset.html
(defn halton [index base]
  (loop [index index
         fraction 1
         result 0]
    (if (<= index 0)
      result
      (let [fract (/ fraction base)]
        (recur (Math/floor (/ index base))
               fract
               (+ result (* fract (mod index base))))))))

(comment (for [i (range 32)]
           (halton i 7)))

;; how to pick good prime pairs?
(defn halton-prime-pair [lower upper]
  (let [primes (sm/primes-between lower upper)
        p1 (dr/rand-nth primes)]
    [p1 (dr/rand-nth (remove #{p1 (+ p1 2) (- p1 2)} primes))]))

(defn halton-sequence [bounds [p1 p2] n]
  (for [i (range n)
        :let [k (+ i 100)]]
    (g/unmap-point bounds (gv/vec2 (halton k p1) (halton k p2)))))

(def modes
  {:random-points random-points
   :random-cells random-cells
   :random-cell-jitter random-cell-jitter
   :poisson-disc-sampling poisson-disc-sampling
   :halton-sequence halton-sequence} )

;; Parameterize random somehow?
(defprotocol ISamplePoint
  (sample-point-at [_ t] [_ u v])
  (sample-point-bounds [_])
  (sample-point-inside [_]))

(extend-type Circle2
  ISamplePoint
  (sample-point-at
    ([{:keys [p r]} t] (v/+polar p r (* eq/TAU t)))
    ([{:keys [p r]} u v]
     (v/+polar p (* r (Math/sqrt u)) (* eq/TAU v))))
  (sample-point-inside [_]
    (sample-point-at _ (dr/random) (dr/random)))
  (sample-point-bounds [{:keys [p r]}]
    (v/+polar p r (dr/random-tau))))

(extend-type Rect2
  ISamplePoint
  (sample-point-at
    ([bounds t]
     (g/point-at bounds t))
    ([bounds u v]
     (g/unmap-point bounds (gv/vec2 u v))))
  (sample-point-inside [bounds]
    (sample-point-at bounds (dr/random) (dr/random)))
  (sample-point-bounds [bounds]
    (sample-point-at bounds (dr/random))))

(extend-type Triangle2
  ISamplePoint
  (sample-point-at
    ([{:keys [points]} t]
     (gu/point-at t (conj points (first points))))
    ([{:keys [points]} u v]
     (let [s (min u v)
           t (max u v)
           [a b c] points]
       (tm/+ (tm/* a s)
             (tm/* b (- t s))
             (tm/* c (- 1 t))))))
  (sample-point-inside [_]
    (sample-point-at _ (dr/random) (dr/random)))
  (sample-point-bounds [_]
    (sample-point-at _ (dr/random))))

(comment
  (for [u (range 0 1 0.2)
        v (range 0 1 0.2)]
    [[u v] (sample-point-at (gt/triangle2 [0 0] [10 10] [10 5]) u v)])

  (repeatedly 20 #(sample-point-inside (gt/triangle2 [0 0] [10 0] [0 10]))))

(extend-type Polygon2
  ISamplePoint
  (sample-point-at [{:keys [points]} t]
    (gu/point-at t (conj points (first points))))
  (sample-point-inside [_]
    (->> (g/tessellate _)
         (map gt/triangle2)
         (dr/weighted-by triangle/signed-area)
         sample-point-inside))
  (sample-point-bounds [_]
    (sample-point-at _ (dr/random))))

(comment
  (repeatedly 20 #(sample-point-inside (gp/polygon2 [0 0] [10 0] [10 10] [0 10])))
  (repeatedly 20 #(sample-point-bounds (gp/polygon2 [0 0] [10 0] [10 10] [0 10]))))

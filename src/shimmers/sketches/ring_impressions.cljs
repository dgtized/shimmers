(ns shimmers.sketches.ring-impressions
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; use distorted tree-ring rendering but with connectives ala constellations

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn candidate-circles [bounds]
  (let [small-bounds (g/scale-size bounds 0.75)
        rules {:bounds bounds
               :candidates 5
               :gen-circle (fn [] (let [pt (rp/sample-point-inside small-bounds)]
                                   (gc/circle pt (min (* 0.9 (poly/dist-to-closest-point bounds pt))
                                                      (* 0.3 height)))))
               :spacing (* 0.025 height)}]
    (pack/circle-pack [] rules)))

(defn gen-circles [bounds]
  (some (fn [circles] (when (and (> (count circles) 2)
                                (> (reduce + (map g/area circles))
                                   (* 0.33 (g/area bounds))))
                       circles))
        (repeatedly 200 #(candidate-circles bounds))))

;; filter for overlapping circle in between?
(defn connectives [circles]
  (let [pts (map :p circles)]
    (for [[p q] (take (dec (count circles)) (cs/all-pairs pts))]
      [(gl/line2 p q)
       (->> (tm/norm-range (/ (g/dist p q) 3.0))
            (mapv (fn [t] (tm/mix p q t)))
            (lines/split-segments 0.33))])))

;; TODO use exit-bands to calculate range bands for each ring. Should help with
;; not showing the obviously starting line at the zero angle
(defn exit-bands [center exits]
  (->> exits
       (map (fn [exit] (g/heading (tm/- exit center))))
       sort
       cs/pair-cycle))

(defn noise-pos [seed]
  (fn [scale pos]
    (dr/noise-at-point-01 seed scale pos)))

(defn ring [noise p r n-r n displace exits]
  (let [split-chance (+ 0.25 (* 0.75 (noise 0.05 (gv/vec2 0.0 r))))
        exit-angles (map (fn [e] (g/heading (tm/- e p))) exits)
        ;; TODO: split on exits with a margin
        groups (->> (for [t (range 0 eq/TAU (/ eq/TAU n))
                          :when (let [v (every? (fn [e] (> (sm/radial-distance t e)
                                                          (* 0.15 (math/sqrt (- 1.0 n-r)))))
                                                exit-angles)]
                                  v)]
                      (let [pos (v/polar r t)
                            n (noise 0.0035 pos)]
                        (v/+polar pos displace (* eq/TAU n))))
                    (partition 2 1)
                    (partition-by (fn [[p q]] (> (g/dist p q) 5)))
                    (filter (fn [group] (> (count group) 1)))
                    (map (fn [group] (map first group))))]
    (mapcat (fn [points]
              (map (fn [segment] (g/translate segment p))
                   (lines/split-segments split-chance points)))
            groups)))

(defn tree-rings [noise {p :p radius :r} exits]
  (mapcat (fn [r]
            (ring noise
                  p
                  (* r radius)
                  r
                  (int (math/pow 30 (+ 1 r)))
                  (math/ceil (* radius 0.025 (+ 1 r)))
                  exits))
          (dr/gaussian-range 0.025 0.012)))

(defn exits [{center :p} lines]
  (mapcat (fn [line]
            (let [[p q] (g/vertices line)]
              (cond (tm/delta= center p)
                    [q]
                    (tm/delta= center q)
                    [p]
                    :else
                    [])))
          lines))

(defn shapes []
  (let [bounds (csvg/screen width height)
        circles (gen-circles bounds)
        lines (connectives circles)]
    (concat (mapcat second lines)
            (mapcat (fn [circle]
                      (tree-rings (noise-pos (dr/noise-seed)) circle
                                  (exits circle (map first lines))))
                    circles))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "white"
     :stroke-width 0.75}
    (shapes)))

(sketch/definition ring-impressions
  {:created-at "2024-06-30"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))

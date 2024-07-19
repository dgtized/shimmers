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
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.intersection :as isec]
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

(defn planar-pairs [circles pairs]
  (letfn [(planar? [circles xs segment]
            (let [[p q] segment]
              (and (not-any? (fn [line] (when (isec/segment-intersect line segment)
                                         segment))
                             xs)
                   (not-any? (fn [circle]
                               (when-let [isec (isec/circle-ray (g/scale-size circle 1.1) p q)]
                                 (when (= :impale (:type isec))
                                   circle)))
                             circles))))]
    (reduce (fn [xs segment]
              (if (planar? circles xs segment)
                (conj xs segment)
                xs))
            []
            (cs/all-pairs pairs))))

(defn noise-pos [seed]
  (fn [scale pos]
    (dr/noise-at-point-01 seed scale pos)))

;; filter for overlapping circle in between?
(defn connectives [circles]
  (for [[p q] (->> circles
                   (map (fn [{:keys [p r] :as c}]
                          (let [n ((noise-pos (:noise (meta c))) 0.0035 (gv/vec2))]
                            (v/+polar p (* 0.025 r) (* eq/TAU n)))))
                   (planar-pairs circles))]
    [(gl/line2 p q)
     (->> (tm/norm-range (/ (g/dist p q) 3.0))
          (mapv (fn [t] (tm/mix p q t)))
          (lines/split-segments 0.33))]))

(defn candidate-circles [bounds]
  (let [small-bounds (g/scale-size bounds 0.75)
        rules {:bounds bounds
               :candidates 5
               :gen-circle
               (fn [] (let [pt (rp/sample-point-inside small-bounds)]
                       (gc/circle pt (min (* 0.9 (poly/dist-to-closest-point bounds pt))
                                          (* 0.3 height)))))
               :spacing (* 0.025 height)}]
    (pack/circle-pack [] rules)))

(defn gen-circles [bounds]
  (some (fn [{:keys [circles] :as candidate}]
          (when (and (> (count circles) 2)
                     (> (reduce + (map g/area circles))
                        (* 0.4 (g/area bounds))))
            candidate))
        (repeatedly 200
                    (fn []
                      (let [circles (map (fn [c] (vary-meta c assoc :noise (dr/noise-seed)))
                                         (candidate-circles bounds))]
                        {:circles circles
                         :lines (connectives circles)})))))

;; TODO use exit-bands to calculate range bands for each ring. Should help with
;; not showing the obviously starting line at the zero angle
(defn exit-bands [center exits]
  (->> exits
       (map (fn [exit] (g/heading (tm/- exit center))))
       sort
       cs/pair-cycle
       (mapv (fn [[a b]] (if (> a b) [a (+ b eq/TAU)] [a b])))))

(defn ring [noise p r n margin displace bands]
  (let [split-chance (+ 0.25 (* 0.75 (noise 0.05 (gv/vec2 0.0 r))))]
    (->> (for [[a b] bands]
           (->>  (tm/norm-range (math/ceil (* n (/ (- b a) eq/TAU))))
                 (map (fn [t]
                        (let [angle (tm/mix* (+ a margin) (- b margin) t)
                              pos (v/polar r angle)]
                          (v/+polar pos displace (* eq/TAU (noise 0.0035 pos))))))))
         (mapcat (fn [points]
                   (map (fn [segment] (g/translate segment p))
                        (lines/split-segments split-chance points)))))))

(defn tree-rings [{p :p radius :r :as c} exits]
  (let [bands (exit-bands p exits)
        noise (noise-pos (:noise (meta c)))]
    (mapcat (fn [r]
              (ring noise
                    p
                    (* r radius)
                    (int (math/pow 30 (+ 1 r)))
                    (* 0.075 (+ 1 (math/sqrt (- 1.0 r))))
                    (math/ceil (* radius 0.025 (+ 1 r)))
                    bands))
            (dr/gaussian-range 0.025 0.012))))

(defn exits [{center :p} lines]
  (mapcat (fn [line]
            (let [[p q] (g/vertices line)]
              (cond (tm/delta= center p 8.0)
                    [q]
                    (tm/delta= center q 8.0)
                    [p]
                    :else
                    [])))
          lines))

(defn shapes []
  (let [bounds (csvg/screen width height)
        {:keys [circles lines]} (gen-circles bounds)]
    (concat (mapcat second lines)
            (mapcat (fn [circle]
                      (tree-rings circle (exits circle (map first lines))))
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

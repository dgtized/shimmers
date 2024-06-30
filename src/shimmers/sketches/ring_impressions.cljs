(ns shimmers.sketches.ring-impressions
  (:require
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [shimmers.common.sequence :as cs]
   [thi.ng.geom.line :as gl]))

;; use distorted tree-ring rendering but with connectives ala constellations

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn candidate-seeds [bounds]
  (let [small-bounds (g/scale-size bounds 0.75)
        rules {:bounds bounds
               :candidates 5
               :gen-circle (fn [] (let [pt (rp/sample-point-inside small-bounds)]
                                   (gc/circle pt (min (* 0.9 (poly/dist-to-closest-point bounds pt))
                                                      (* 0.3 height)))))
               :spacing (* 0.025 height)}]
    (pack/circle-pack [] rules)))

(defn gen-seeds [bounds]
  (some (fn [circles] (when (and (> (count circles) 2)
                                (> (reduce + (map g/area circles))
                                   (* 0.33 (g/area bounds))))
                       circles))
        (repeatedly 200 #(candidate-seeds bounds))))

;; filter for overlapping circle in between?
(defn connectives [seeds]
  (let [pts (map :p seeds)]
    (for [[p q] (take (dec (count seeds)) (cs/all-pairs pts))]
      (gl/line2 p q))))

(defn shapes []
  (let [bounds (csvg/screen width height)
        seeds (gen-seeds bounds)]
    (concat seeds
            (connectives seeds))))

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

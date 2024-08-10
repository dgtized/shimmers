(ns shimmers.sketches.ley-lines
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.line :as gl]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-path [bounds start-fn seed scale lifespan]
  (fn []
    (let [path
          (->> [(start-fn) (gv/vec2)]
               (iterate
                (fn [[p v]]
                  (let [noise (dr/noise-at-point-01 seed scale p)
                        v' (tm/* (tm/+ v (v/polar (/ 100 height) (* noise eq/TAU))) 0.85)]
                    [(tm/+ p v) v'])))
               (take (lifespan))
               (take-while (fn [[p _v]] (g/contains-point? bounds p)))
               (map first))]
      (when (and (seq path) (> (count path) 1))
        (csvg/group {}
          (csvg/path (csvg/segmented-path path)
                     {:stroke-width 0.5})
          (let [stops (dr/gaussian-range 0.125 0.05)]
            (map (fn [t] (gc/circle (g/point-at (gl/linestrip2 path) t) 1.5)) stops)))))))

(defn shapes [bounds]
  (let [start (fn [] (rp/sample-point-inside (gc/circle (rv 0.5 0.5) (* 0.4 height))))
        scale 0.001
        seed (dr/noise-seed)
        lifespan (fn [] (int (dr/gaussian 100 10)))]
    (repeatedly
     384
     (make-path (g/scale-size bounds 0.95) start seed scale lifespan))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.75}
    (shapes (csvg/screen width height))))

(sketch/definition ley-lines
  {:created-at "2024-08-10"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))

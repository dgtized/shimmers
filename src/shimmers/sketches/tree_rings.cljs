(ns shimmers.sketches.tree-rings
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; inspired by https://gorillasun.de/blog/radial-perlin-noise-and-generative-tree-rings

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn path-segment [points]
  (csvg/path
   (concat [[:M (first points)]]
           (mapv (fn [p] [:L p]) (rest points)))))

(defn path-ring [points]
  (csvg/path
   (concat [[:M (first points)]]
           (mapv (fn [p] [:L p]) (rest points))
           [[:Z]])))

(defn ring [seed r n]
  (let [points (for [t (range 0 eq/TAU (/ eq/TAU n))]
                 (let [p (g/as-cartesian (gv/vec2 r t))
                       noise (dr/noise-at-point seed 0.005 p)]
                   (tm/+ p (g/as-cartesian (gv/vec2 (* r 0.05) (* eq/TAU noise))))))]
    (if (dr/chance 0.4)
      [(path-ring points)]
      (->> points
           (partition-by (fn [_] (dr/chance 0.8)))
           (filter #(> (count %) 1))
           (mapv path-segment)))))

(defn shapes []
  (let [radius (int (/ height 2.05))
        seed (gv/vec2 (dr/random 100) (dr/random 100))]
    (mapcat (fn [r]
              (ring seed
                    (* r radius)
                    (int (Math/pow 20 (+ 1 (* 0.75 r))))))
            (dr/density-range 0.0075 0.05))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
    [[:g {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes)]]))

(sketch/definition tree-rings
  {:created-at "2022-10-22"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :tree-rings)
              "sketch-host"))

(ns shimmers.sketches.tree-rings
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; inspired by https://gorillasun.de/blog/radial-perlin-noise-and-generative-tree-rings

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn ring [seed r n displace]
  (let [split-chance (+ 0.25 (* 0.75 (dr/noise-at-point-01 seed 0.035 (gv/vec2 0.0 r))))
        base-t (dr/random-tau)
        points (for [t (range 0 eq/TAU (/ eq/TAU n))]
                 (let [p (g/as-cartesian (gv/vec2 r (+ t base-t)))
                       noise (dr/noise-at-point-01 seed 0.002 p)]
                   (tm/+ p (g/as-cartesian (gv/vec2 displace (* eq/TAU noise))))))]
    (lines/split-segments split-chance points)))

(defn shapes []
  (let [radius (int (/ height 2.1))
        seed (dr/noise-seed)]
    (mapcat (fn [r]
              (ring seed
                    (* r radius)
                    (int (math/pow 30 (+ 1 r)))
                    (math/ceil (* radius 0.025 (+ 1 r)))))
            (dr/gaussian-range 0.01 0.012))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.66}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes))))

(sketch/definition tree-rings
  {:created-at "2022-10-22"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount (usvg/page sketch-args scene)))

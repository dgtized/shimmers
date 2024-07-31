(ns shimmers.sketches.pendulum-sway
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn plot [{:keys [p r]} lambda1 lambda2 dx dy]
  (let [limit 90]
    (for [t (range 0 (* limit eq/TAU) 0.05)]
      (let [dampen1 (math/exp (* (- lambda1) t))
            dampen2 (math/exp (* (- lambda2) t))]
        (gc/circle (tm/+ p
                         (tm/+ (gv/vec2 (* r dampen1 (math/cos (* dampen2 dx t)))
                                        (* r dampen1 (math/sin (* dampen2 dy t))))
                               (v/polar (* 0.04 r dampen2) (* math/PI t))))
                   1.0)))))

(defn scene [{:keys [scene-id]}]
  (let [dx (+ (dr/random-int 1 4) (dr/random -0.01 0.01))
        dy (+ (dr/random-int 1 4) (dr/random -0.01 0.01))]
    (println dx dy)
    (csvg/svg-timed
      {:id scene-id
       :width width
       :height height
       :stroke "black"
       :fill "white"
       :stroke-width 0.5}
      (plot {:p (rv 0.5 0.5) :r (* 0.475 height)}
            0.004 0.003 dx dy))))

(sketch/definition pendulum-sway
  {:created-at "2024-07-31"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))

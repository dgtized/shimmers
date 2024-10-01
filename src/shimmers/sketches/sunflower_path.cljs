(ns shimmers.sketches.sunflower-path
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [{:keys [points alpha]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.45 (min width height))
        exterior (min (int (* alpha (math/sqrt points))) points)
        interior (- points exterior)]
    (csvg/path (into [[:M center]]
                     (for [i (range points)
                           :let [r (if (< i interior) (/ (float i) (inc interior)) 1.0)
                                 theta (* eq/TAU (/ i (eq/sqr tm/PHI)))]]
                       [:L (v/+polar center (* r radius) theta)])))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
                  (shapes {:points 256 :alpha 1.0})))

(sketch/definition sunflower-path
  {:created-at "2024-09-30"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))

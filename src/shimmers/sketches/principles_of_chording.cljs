(ns shimmers.sketches.principles-of-chording
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; https://en.wikipedia.org/wiki/Maurer_rose

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [{:keys [n d]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.45 height)
        path (for [i (range 361)
                   :let [k (tm/radians (* d i))]]
               (v/+polar center (* radius (math/sin (* n k))) k))]
    (csvg/path (csvg/segmented-path path))))

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes params)))

(defn param-gen []
  {:n ((if (dr/chance 0.5) math/round identity)
       (dr/random 2 16))
   :d ((if (dr/chance 0.3) math/round identity)
       (dr/random 2 128))})

(defn explanation [{:keys [params]}]
  [:div (debug/pre-edn params)])

(sketch/definition principles-of-chording
  {:created-at "2026-04-14"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (-> sketch-args
       (usvg/with-param-gen param-gen)
       (usvg/with-explanation explanation)
       (usvg/page scene))))

(ns shimmers.sketches.principles-of-chording
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; https://en.wikipedia.org/wiki/Maurer_rose

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn maurer-rose [{:keys [n d]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)
        path (for [i (range 361)
                   :let [k (tm/radians (* d i))]]
               (v/+polar center (* radius (math/sin (* n k))) k))]
    (csvg/path (csvg/segmented-path path))))

(defn chorded [{:keys [n d]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)]
    (for [i (range 721)
          :let [k (* n i)]]
      (gl/line2 (v/+polar center radius
                          (tm/radians i))
                (v/+polar center radius
                          (+ (tm/radians i) (mod (tm/radians k) d)))))))

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (({:maurer-rose maurer-rose
       :chorded chorded} (:method params))
     params)))

(defn param-gen []
  (let [method (dr/weighted {:maurer-rose 1
                             :chorded 1})]
    (case method
      :maurer-rose
      {:method method
       :n ((if (dr/chance 0.5) math/round identity)
           (dr/random 2 16))
       :d ((if (dr/chance 0.3) math/round identity)
           (dr/random 2 128))}
      :chorded
      {:method method
       :n ((if (dr/chance 0.4) math/round identity)
           (min (dr/random 0.5 9)
                (dr/random 0.5 9)))
       :d (min (dr/random 1.0 eq/TAU)
               (dr/random 1.0 eq/TAU))})))

(defn explanation [{:keys [params]}]
  [:div.evencols (debug/pre-edn params)])

(sketch/definition principles-of-chording
  {:created-at "2026-04-14"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (-> sketch-args
       (usvg/with-param-gen param-gen)
       (usvg/with-explanation explanation)
       (usvg/page scene))))

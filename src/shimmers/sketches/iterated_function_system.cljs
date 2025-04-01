(ns shimmers.sketches.iterated-function-system
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn gen-params []
  {:a (- (dr/random-tau) math/PI)
   :b (- (dr/random-tau) math/PI)
   :c (- (dr/random-tau) math/PI)
   :d (- (dr/random-tau) math/PI)})

(defn dejong-ifs [a b c d]
  (fn [[x y]]
    (gv/vec2 (- (math/sin (* a y)) (math/cos (* b x)))
             (- (math/sin (* c x)) (math/cos (* d y))))))

(defn shapes [{:keys [a b c d]}]
  (let [ifs (dejong-ifs a b c d)]
    (for [seed (repeatedly 128 dr/randvec2)
          point (take 128 (iterate ifs seed))]
      (gc/circle (tm/+ (rv 0.5 0.5) (tm/* point (* height 0.225)))
                 0.5))))

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "none"
                   :fill "black"}
    (shapes params)))

(sketch/definition iterated-function-system
  {:created-at "2025-03-31"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (usvg/page (assoc sketch-args
                     :params (gen-params))
              scene)))

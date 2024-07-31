(ns shimmers.sketches.pendulum-sway
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
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn f1 [r t {:keys [lambda1 lambda2 dx dy]}]
  (let [dampen1 (math/exp (* (- lambda1) t))
        dampen2 (math/exp (* (- lambda2) t))]
    (tm/+ (gv/vec2 (* r dampen1 (math/cos (* dampen2 dx t)))
                   (* r dampen1 (math/sin (* dampen2 dy t))))
          (v/polar (* 0.04 r dampen2) (* 3 t)))))

(defn f2 [r t {:keys [lambda1 lambda2 dx dy]}]
  (let [dampen1 (math/exp (* (- lambda1) t))
        dampen2 (math/exp (* (- lambda2) t))]
    (tm/+ (gv/vec2 (* r dampen1 (math/cos (+ (* 2 dampen2) (* dx t))))
                   (* r dampen1 (math/sin (+ (* 2 dampen2) (* dy t)))))
          (v/polar (* 0.04 r dampen2) (* 3 t)))))

(defn plot [{:keys [p r]} {:keys [f] :as params}]
  (let [limit 90]
    (for [t (range 0 (* limit eq/TAU) 0.05)]
      (gc/circle (tm/+ p (f r t params))
                 (dr/gaussian 1.1 0.2)))))

(defn gen-parameters []
  {:f (dr/rand-nth [f1 f2])
   :dx (+ (dr/random-int 1 6) (dr/random -0.01 0.01))
   :dy (+ (dr/random-int 1 6) (dr/random -0.01 0.01))
   :lambda1 0.004
   :lambda2 0.003})

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (plot {:p (rv 0.5 0.5) :r (* 0.475 height)} params)))

(defn ui-controls [{:keys [params]}]
  [:div
   [debug/pre-edn params]])

(sketch/definition pendulum-sway
  {:created-at "2024-07-31"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page (assoc sketch-args
                                :params (gen-parameters)
                                :explanation ui-controls) scene)))

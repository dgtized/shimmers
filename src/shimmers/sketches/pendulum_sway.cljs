(ns shimmers.sketches.pendulum-sway
  (:require
   [clojure.math :as math]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn alpha [r t {:keys [lambda1 lambda2 dx dy]}]
  (let [dampen1 (math/exp (* (- lambda1) t))
        dampen2 (math/exp (* (- lambda2) t))]
    (tm/+ (gv/vec2 (* r dampen1 (math/cos (* dampen2 dx t)))
                   (* r dampen1 (math/sin (* dampen2 dy t))))
          (v/polar (* 0.04 r dampen2) (* 3 t)))))

(defn beta [r t {:keys [lambda1 lambda2 dx dy]}]
  (let [dampen1 (math/exp (* (- lambda1) t))
        dampen2 (math/exp (* (- lambda2) t))]
    (tm/+ (gv/vec2 (* r dampen1 (math/cos (+ (* 2 dampen2) (* dx t))))
                   (* r dampen1 (math/sin (+ (* 2 dampen2) (* dy t)))))
          (v/polar (* 0.04 r dampen2) (* 3 t)))))

;; experiment with frequency modulation
(defn gamma [r t {:keys [lambda1 lambda2 dx dy gamma-rate]}]
  (let [dampen1 (math/exp (* (- lambda1) t))
        dampen2 (math/exp (* (- lambda2) t))
        fx (+ dx (* 0.001 (math/sin (* gamma-rate t))))
        fy (+ dy (* 0.001 (math/cos (* gamma-rate t))))]
    (tm/+ (gv/vec2 (* r dampen1 (math/cos (+ (* 6 dampen2) (* fx t))))
                   (* r dampen1 (math/sin (+ (* 6 dampen2) (* fy t)))))
          (v/polar (* 0.04 r dampen2) (* 4 t)))))

(def functions
  {:alpha alpha
   :beta beta
   :gamma gamma})

(defn plot [{:keys [p r]} {:keys [select-fn] :as params}]
  (let [limit 100
        f (functions select-fn)]
    (for [t (cs/range-series 0 (* limit eq/TAU)
                             (fn [t] (* 0.0225 (math/exp (* 0.003 t)))))]
      (gc/circle (tm/+ p (f r t params))
                 (+ 1.3 (* 0.7
                           (math/exp (* -0.001 t))
                           (math/sin (+ (* 9 t)))))))))

(defn triangulate [circles]
  (map-indexed (fn [i {:keys [p] :as c}]
                 (if (or (= 0 (mod i 5)) (= 0 (mod i 7)))
                   (triangle/inscribed-equilateral c (g/heading (tm/- (rv 0.5 0.5) p)))
                   c))
               circles))

(defn gen-parameters []
  {:select-fn (dr/weighted [[:alpha 1.0] [:beta 2.0] [:gamma 1.0]])
   :dx (+ (dr/random-int 1 6) (dr/random -0.01 0.01))
   :dy (+ (dr/random-int 1 6) (dr/random -0.01 0.01))
   :lambda1 0.004
   :lambda2 0.003
   :gamma-rate (dr/weighted {0.25 1 0.5 2 0.75 1 1.25 1})})

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (triangulate (plot {:p (rv 0.5 0.5) :r (* 0.475 height)} params))))

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

(ns shimmers.sketches.carrier-wave
  (:require
   [clojure.math :as math]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.fraction :as fraction]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wobble :as mw :refer [R]]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn generate-points [[a b c d k] center radius t]
  (for [s (tm/norm-range 1024)
        :let [noise (eq/unit-sin (+ (* 0.7 t) (* 0.5 math/PI s) (math/sin (+ (* 0.2 t) s))))]]
    (-> (gv/vec2)
        (tm/+ (R (+ a (eq/unit-cos (* 0.2 t))) (math/cos (* 0.5 t)) 1.2 s))
        (tm/+ (R (+ c (eq/unit-sin (* 0.8 t))) (* -2 k (math/sin (* 0.25 t))) 0.5 s))
        (tm/+ (R (+ b (eq/unit-sin (* -0.2 t))) (math/sin (* -0.5 t)) 1.1 s))
        (tm/+ (R (+ d (math/tan (+ k (* 0.125 t)))) (dr/gaussian 0.0 (* 0.0066 noise)) 1.0 s))
        (tm/* radius)
        (tm/+ center))))

(defn draw
  [ui-state _fs ctx [width height] ms]
  (let [t (* 0.001 ms)
        center (gv/vec2 (* 0.5 width) (* 0.5 height))
        {:keys [params k]} @ui-state
        [a b c d] (map :value params)
        points (generate-points [a b c d (* 1 k)] center (* 0.133 height) (* 0.5 t))]
    (if (:inverted @ui-state)
      (do (canvas/fill-style ctx "rgb(0,0,0)")
          (.fillRect ctx 0 0 width height)
          (canvas/stroke-style ctx "rgb(255,255,255)"))
      (canvas/stroke-style ctx "rgb(0,0,0)"))
    (canvas/line-width ctx 1.0)
    (canvas/stroke-path ctx points)
    ctx))

;; TODO: ensure fractions have a min/max value?
(defn controls [ui-state]
  (let [bounds {:lower -10 :upper 10}]
    [:div
     [fraction/control ui-state "A" [:params 0] bounds]
     [fraction/control ui-state "B" [:params 1] bounds]
     [fraction/control ui-state "C" [:params 2] bounds]
     [fraction/control ui-state "D" [:params 3] bounds]
     [ctrl/checkbox ui-state "Inverted" [:inverted]]]))

(defn page []
  (let [ui-state
        (ctrl/state
         {:params (vec (repeatedly 4 #(fraction/make (str (dr/random-int -3 3)))))
          :k (dr/random)})
        {:keys [canvas-state attributes]}
        (canvas/make-state
         {:width 800
          :height 600
          :draw (partial #'draw ui-state)}
         {:width-pct 0.7})]
    (fn []
      [sketch/with-explanation
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]
       [ctrl/container
        [controls ui-state]]])))

(sketch/definition carrier-wave
  {:created-at "2024-02-12"
   :tags #{}
   :type :canvas}
  (ctrl/mount (page)))

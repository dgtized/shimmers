(ns shimmers.sketches.carrier-wave
  (:require
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn R [f p a s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn generate-points [[a b c d k] center radius t]
  (for [s (tm/norm-range 1024)
        :let [noise (eq/unit-sin (+ (* 0.7 t) (* 0.5 Math/PI s) (Math/sin (+ (* 0.2 t) s))))]]
    (-> (tm/+ (R (+ a (eq/unit-cos (* 0.2 t))) (Math/cos (* 0.5 t)) 1.2 s)
              (R (+ b (eq/unit-sin (* -0.2 t))) (Math/sin (* -0.5 t)) 1.1 s)
              (tm/+ (R (+ c (eq/unit-sin (* 0.8 t))) (* -2 k (Math/sin (* 0.25 t))) 0.5 s)
                    (R (+ d (Math/tan (+ k (* 0.125 t)))) (dr/gaussian 0.0 (* 0.0066 noise)) 1.0 s)))
        (tm/* radius)
        (tm/+ center))))

(defn draw
  [ui-state _fs ctx [width height] ms]
  (let [t (* 0.001 ms)
        center (gv/vec2 (* 0.5 width) (* 0.5 height))
        {:keys [params k]} @ui-state
        [a b c d] params
        points (generate-points [a b c d (* 1 k)] center (* 0.133 height) (* 0.5 t))]
    (if (:inverted @ui-state)
      (do (canvas/fill-style ctx "rgb(0,0,0)")
          (.fillRect ctx 0 0 width height)
          (canvas/stroke-style ctx "rgb(255,255,255)"))
      (canvas/stroke-style ctx "rgb(0,0,0)"))
    (canvas/line-width ctx 1.0)
    (canvas/stroke-path ctx points)
    ctx))

;; TODO: use fraction controls
(defn controls [ui-state]
  (let [sr [-7 7 0.25]]
    [:div
     (ctrl/numeric ui-state "A" [:params 0] sr)
     (ctrl/numeric ui-state "B" [:params 1] sr)
     (ctrl/numeric ui-state "C" [:params 2] sr)
     (ctrl/numeric ui-state "D" [:params 3] sr)
     (ctrl/checkbox ui-state "Inverted" [:inverted])]))

(defn page []
  (let [ui-state
        (ctrl/state
         {:params (vec (repeatedly 4 #(dr/random-int -3 3)))
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

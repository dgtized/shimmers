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

(defn setup [_cv]
  {:a (dr/random-int 1 4)
   :b (dr/random-int 1 4)
   :c (dr/random-int 1 4)})

(defn update-state [state _dims _ms]
  state)

(defn R [f p a s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn generate-points [[a b c] center height t]
  (for [s (tm/norm-range 512)]
    (-> (tm/+ (R (+ a (eq/unit-cos (* 0.2 t))) (eq/unit-cos (* 0.5 t)) 1.2 s)
              (R (+ b (eq/unit-sin (* -0.2 t))) (eq/unit-sin (* -0.5 t)) 1.1 s)
              (R (+ c (Math/tan (* 0.15 t))) (dr/gaussian 0.0 0.005) 1.0 s))
        (tm/* (* 0.1 height))
        (tm/+ center ))))

(defn draw [{:keys [a b c]} ctx [width height] ms]
  (let [t (* 0.001 ms)
        center (gv/vec2 (* 0.5 width) (* 0.5 height))
        points (generate-points [a b c] center height t)]
    (canvas/line-width ctx 2.0)
    (.beginPath ctx)
    (canvas/move-to ctx (first points))
    (doseq [p (rest points)]
      (canvas/line-to ctx p))
    (canvas/stroke ctx)
    ctx))

(defn page []
  (let [{:keys [canvas-state attributes]}
        (canvas/make-state
         {:width 800
          :height 600
          :setup #'setup
          :update #'update-state
          :draw #'draw}
         {:width-pct 0.7})]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]])))

(sketch/definition carrier-wave
  {:created-at "2024-02-12"
   :tags #{}
   :type :canvas}
  (ctrl/mount (page)))

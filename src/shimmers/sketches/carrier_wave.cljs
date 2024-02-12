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
  (let [s {:params (repeatedly 4 #(dr/random-int -3 3))
           :k (dr/random)}]
    (println s)
    s))

(defn update-state [state _dims _ms]
  state)

(defn R [f p a s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn generate-points [[a b c d k] center radius t]
  (for [s (tm/norm-range 512)]
    (-> (tm/+ (R (+ a (eq/unit-cos (* 0.2 t))) (Math/cos (* 0.5 t)) 1.2 s)
              (R (+ b (eq/unit-sin (* -0.2 t))) (Math/sin (* -0.5 t)) 1.1 s)
              (tm/+ (R (+ c (eq/unit-sin (* 0.8 t))) (* 2 k (Math/sin (* 0.25 t))) 0.5 s)
                    (R (+ d (Math/tan (+ k (* 0.15 t)))) (dr/gaussian 0.0 0.005) 1.0 s)))
        (tm/* radius)
        (tm/+ center))))

(defn draw-path [ctx points]
  (.beginPath ctx)
  (canvas/move-to ctx (first points))
  (doseq [p (rest points)]
    (canvas/line-to ctx p))
  (canvas/stroke ctx)
  ctx)

(defn draw [{:keys [params k]} ctx [width height] ms]
  (let [t (* 0.001 ms)
        center (gv/vec2 (* 0.5 width) (* 0.5 height))
        [a b c d] params]
    (canvas/line-width ctx 1.0)
    (draw-path ctx (generate-points [a b c d (* 1 k)] center (* 0.133 height) t))
    ;; (canvas/line-width ctx 1.0)
    ;; (draw-path ctx (generate-points [c a b (* 2 k)] center (* 0.099 height) t))
    ;; (canvas/line-width ctx 2.0)
    ;; (draw-path ctx (generate-points [b c a (* 3 k)] center (* 0.033 height) t))
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

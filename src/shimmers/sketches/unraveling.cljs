(ns shimmers.sketches.unraveling
  (:require
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Reminiscent of Yoann Bourgeois "Progress is not Linear" dance

(defn spiral-inside [circle t0 dr dt]
  (->> {:circle circle :t t0 :r (:r circle)}
       (iterate
        (fn [{:keys [circle t r]}]
          (let [r' (* dr r)]
            {:circle (gc/circle (v/+polar (g/point-at circle (/ t eq/TAU)) r' (+ t Math/PI)) r')
             :t (+ t dt)
             :r r'})))
       (take-while (fn [{:keys [r]}] (> r 3.0)))
       (map :circle)))

(defn clockwise-arc [ctx [x y] r t0 t1]
  (.beginPath ctx)
  (.arc ctx x y r t0 t1 nil))

(defn setup [_]
  {:t 0})

(defn update-state [{:keys [t] :as state} _dims _time]
  (update state :t + (+ 0.002 (* 0.018 (eq/unit-cos (+ tm/PHI (/ t tm/PHI)))))))

(defn draw [ {:keys [t]} ctx [width height] _time]
  (.clearRect ctx 0 0 width height)
  (set! (.-lineWidth ctx) (+ (/ 1.0 tm/PHI) (* 0.25 (Math/cos (* 1.33 (+ 0.5 t))))))
  (doseq [{:keys [p r]}
          (spiral-inside (gc/circle (gv/vec2 (* 0.5 width) (* 0.5 height))
                                    (* 0.48 height))
                         (* tm/PHI t)
                         (+ 0.79 (* 0.175 (eq/unit-cos t)))
                         (+ 0.01 (* 0.5 (eq/unit-cos (* tm/PHI t)))))
          :let [[x _] p
                theta0 (- (/ x r) (* 0.15 t))
                dist (tm/smoothstep* 0.48 1.1
                                     (- 1.0
                                        (* 0.5 (eq/unit-cos (- (* 0.3 t) (/ eq/TAU r))))
                                        (* 0.5 (eq/unit-cos (* 0.55 t)))))
                theta1 (- theta0 0.001 (* eq/TAU dist))]]
    (clockwise-arc ctx p r theta0 theta1)
    (canvas/stroke ctx))
  ctx)

(defn page []
  (let [{:keys [canvas-state attributes]}
        (canvas/make-state
         {:width 800 :height 600
          :setup #'setup
          :update #'update-state
          :draw #'draw}
         {:height-pct 0.9})]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]])))

(sketch/definition unraveling
  {:created-at "2023-02-02"
   :type :canvas
   :tags #{}}
  (ctrl/mount (page) "sketch-host"))

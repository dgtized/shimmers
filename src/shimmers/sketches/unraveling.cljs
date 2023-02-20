(ns shimmers.sketches.unraveling
  (:require
   [reagent.core :as r]
   [shimmers.common.framerate :as framerate]
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

(defn update-state [{:keys [t] :as state}]
  (update state :t + (+ 0.003 (* 0.017 (eq/unit-cos (+ tm/PHI (/ t tm/PHI)))))))

(defn circle [ctx {[x y] :p r :r}]
  (doto ctx
    .beginPath
    (.arc x y r 0 eq/TAU false)
    .stroke))

(defn draw-frame [ctx width height {:keys [t]}]
  (.clearRect ctx 0 0 width height)
  (set! (.-line-width ctx) (/ 1 tm/PHI))
  (doseq [c (spiral-inside (gc/circle (gv/vec2 (* 0.5 width) (* 0.5 height))
                                      (* 0.48 height))
                           (* tm/PHI t)
                           (+ 0.79 (* 0.175 (eq/unit-cos t)))
                           (+ 0.01 (* 0.5 (eq/unit-cos (* tm/PHI t)))))]
    (circle ctx c))
  ctx)

(defn do-frame []
  (let [frame-state (atom {:t 0})]
    (fn [canvas-el canvas-state]
      (let [measure-frames! (framerate/sampler)]
        (canvas/on-animated-frame
         {:delay 0}
         (fn [t]
           (measure-frames! t)
           (let [{:keys [width height]} @canvas-state
                 ctx (canvas/scale-dpi canvas-el [width height])]
             (swap! frame-state update-state)
             (draw-frame ctx width height @frame-state))))))))

(defn page []
  (let [canvas-state (r/atom {:width 800 :height 600})
        toggle-fs (fn [] (canvas/toggle-full-screen! canvas-state
                                                    {:height-pct 0.9}))
        attributes {:class "canvas-frame"
                    :on-double-click toggle-fs}]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state (do-frame)]])))

(sketch/definition unraveling
  {:created-at "2023-02-02"
   :type :canvas
   :tags #{}}
  (ctrl/mount (page) "sketch-host"))

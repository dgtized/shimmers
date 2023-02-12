(ns shimmers.sketches.othello
  (:require
   [helins.canvas :as cv]
   [reagent.core :as r]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]))

;; remember arc-to
(defn arc [ctx x y radius angle0 angle1 anti-clockwise?]
  (let [path (cv/begin ctx)]
    (cv/arc path x y radius angle0 angle1 anti-clockwise?)
    (cv/fill ctx)
    (cv/close ctx)
    ctx))

(defn draw-frame [ctx width height t]
  (let [cols 12
        rows 9]
    (-> ctx
        (cv/clear 0 0 width height)
        (cv/color-fill "#000"))
    (dotimes [i cols]
      (dotimes [j rows]
        (let [a0 (* eq/TAU (eq/unit-cos (* 0.33 t)) (/ 1.0 j))]
          (arc ctx
               (* (/ width cols) i)
               (* (/ height rows) j)
               (* (/ height rows) 0.45)
               a0
               (+ a0 0.1 (* (- eq/TAU 0.1) (eq/unit-sin (+ (/ 1.0 i) (* 0.1 t)))))
               false))))
    ctx))

(defn do-frame []
  (fn [_ canvas canvas-state]
    (let [{:keys [width height]} @canvas-state
          ctx (canvas/scale-dpi canvas [width height])]
      (cv/on-frame
       (fn [t]
         (let [{:keys [width height]} @canvas-state]
           (draw-frame ctx width height (* 0.001 t))))))))

(defn page []
  (let [canvas-state (r/atom {:width 800 :height 600})
        attributes {:class "canvas-frame"}]
    (fn []
      [:div.contained
       [canvas/canvas-frame attributes canvas-state
        (do-frame)]])))

(sketch/definition othello
  {:created-at "2023-02-11"
   :type :canvas
   :tags #{}}
  (ctrl/mount (page) "sketch-host"))

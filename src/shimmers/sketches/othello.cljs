(ns shimmers.sketches.othello
  (:require
   [helins.canvas :as cv]
   [reagent.core :as r]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]))

(defn draw-frame [ctx width height t]
  (let [cols 16
        rows 12]
    (cv/clear ctx 0 0 width height)
    (dotimes [i cols]
      (dotimes [j rows]
        (let [x (* (/ width cols) (+ i 0.5))
              y (* (/ height rows) (+ j 0.5))
              r (* (/ width cols) 0.45)
              a0 (* eq/TAU (eq/unit-cos (+  (/ j rows) (/ i cols) (* 0.45 t))))
              a1 (+ a0
                    (* eq/TAU
                       (eq/unit-sin (+ (/ 1.0 (inc i))
                                       (- 1.0 (/ 1.0 (inc j)))
                                       (* 0.25 t)))))
              b0 (* eq/TAU (eq/unit-cos (+  (/ x width) (/ y height) (* 0.4 t))))
              b1 (+ b0
                    (* eq/TAU
                       (eq/unit-sin (+ (/ 1.0 x)
                                       (/ 1.0 y)
                                       (* 0.2 t)))))
              c0 (* eq/TAU (eq/unit-cos (+  (- 1.0 (/ x width))
                                       (- 1.0 (/ y height))
                                       (* 0.66 t))))
              c1 (+ c0
                    (* eq/TAU
                       (eq/unit-sin (+ (- 1.0 (/ 1.0 x))
                                       (- 1.0 (/ 1.0 y))
                                       (* 0.33 t)))))]
          (cv/arc (cv/begin ctx) x y r a0 a1 false)
          (cv/color-fill ctx "rgba(192,0,32,0.5)")
          (cv/fill ctx)
          (cv/arc (cv/begin ctx) x y r b0 b1 false)
          (cv/color-fill ctx "rgba(0.0,64,200,0.5)")
          (cv/fill ctx)
          (cv/arc (cv/begin ctx) x y r c0 c1 false)
          (cv/color-fill ctx "rgba(240,240,0,0.5)")
          (cv/fill ctx))))
    ctx))

(defn do-frame []
  (fn [_ canvas canvas-state]
    (let [{:keys [width height]} @canvas-state
          ctx (canvas/scale-dpi canvas [width height])]
      (cv/on-frame
       (fn [t]
         (draw-frame ctx width height (* 0.001 t)))))))

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

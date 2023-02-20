(ns shimmers.sketches.othello
  (:require
   [helins.canvas :as cv]
   [reagent.core :as r]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn draw-frame [ctx width height t]
  (let [r (max 30 (int (/ (min width height) 16)))
        cols (int (/ width (* 2.66 r)))
        rows (int (/ height (* 2.66 r)))]
    (cv/clear ctx 0 0 width height)
    (cv/line-width ctx (/ r 3))
    (dotimes [i cols]
      (dotimes [j rows]
        (let [x (* (/ width cols) (+ i 0.5))
              y (* (/ height rows) (+ j 0.5))
              offset (* 0.33 r)
              dx (* offset (/ (Math/sqrt 3) 2))
              dy offset
              a0 (* eq/TAU (eq/unit-cos (+  (/ x width) (/ y height) (* 0.45 t))))
              a1 (+ a0 (* eq/TAU (eq/unit-sin (+ (- 1.0 (/ 1.0 x)) (/ 1.0 y) (* 0.25 t)))))
              b0 (* eq/TAU (eq/unit-cos (+  (/ x width) (- 1.0 (/ y height)) (* 0.41 t))))
              b1 (+ b0 (* eq/TAU (eq/unit-sin (+ (/ 1.0 x) (/ 1.0 y) (* 0.23 t)))))
              c0 (* eq/TAU (eq/unit-cos (+  (- 1.0 (/ x width)) (- 1.0 (/ y height)) (* 0.66 t))))
              c1 (+ c0 (* eq/TAU (eq/unit-sin (+ (/ 1.0 x) (/ 1.0 y) (* 0.37 t)))))
              d0 (* eq/TAU (eq/unit-cos (+  (/ x width) (/ y height) (* 0.37 t))))
              d1 (+ d0 (* eq/TAU (eq/unit-sin (+ (/ 1.0 x) (- 1.0 (/ 1.0 y)) (* 0.27 t)))))]
          (cv/arc (cv/begin ctx) x y (* 0.5 tm/SQRT2 r) a0 a1 false)
          (cv/color-stroke ctx "rgba(0,0,0,0.66)")
          (cv/stroke ctx)
          (cv/arc (cv/begin ctx) x (- y dy) r b0 b1 false)
          (cv/color-fill ctx "rgba(240,0,240,0.4)")
          (cv/fill ctx)
          (cv/arc (cv/begin ctx) (- x dx) (+ y (* 0.5 dy))
                  r c0 c1 false)
          (cv/color-fill ctx "rgba(0.0,240,240,0.4)")
          (cv/fill ctx)
          (cv/arc (cv/begin ctx) (+ x dx) (+ y (* 0.5 dy))
                  r d0 d1 false)
          (cv/color-fill ctx "rgba(240,240,0,0.4)")
          (cv/fill ctx))))
    ctx))

(defn do-frame []
  (fn [canvas-el canvas-state]
    (let [measure-frames! (framerate/sampler)]
      (canvas/on-animated-frame
       {:delay 0}
       (fn [t]
         (measure-frames! t)
         (let [{:keys [width height]} @canvas-state
               ctx (canvas/scale-dpi canvas-el [width height])]
           (draw-frame ctx width height (* 0.001 t))))))))

(defn page []
  (let [canvas-state (r/atom {:width 900 :height 600})
        attributes {:class "canvas-frame"}]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state (do-frame)]
       [:div.contained
        [:div.center
         [:button
          {:on-click (fn [] (canvas/toggle-full-screen!
                            canvas-state {:width-pct 0.7}))}
          "Toggle Fullscreen"]]]])))

(sketch/definition othello
  {:created-at "2023-02-11"
   :type :canvas
   :tags #{}}
  (ctrl/mount (page) "sketch-host"))

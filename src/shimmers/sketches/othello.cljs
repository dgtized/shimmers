(ns shimmers.sketches.othello
  (:require
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn fill-path [ctx color]
  (set! (.-fillStyle ctx) color)
  (.fill ctx)
  ctx)

(defn stroke-path [ctx color]
  (set! (.-strokeStyle ctx) color)
  (.stroke ctx)
  ctx)

(defn turns [t0 t1]
  (let [a0 (* eq/TAU (eq/unit-cos t0))]
    [a0 (+ a0 (* eq/TAU (eq/unit-sin t1)))]))

(defn draw [_ ctx [width height] ms]
  (let [t (* 0.001 ms)
        r (max 30 (int (/ (min width height) 16)))
        cols (int (/ width (* 2.66 r)))
        rows (int (/ height (* 2.66 r)))]
    (canvas/clear ctx width height)
    (canvas/line-width ctx (/ r 3))
    (dotimes [i cols]
      (dotimes [j rows]
        (let [x (* (/ width cols) (+ i 0.5))
              y (* (/ height rows) (+ j 0.5))
              offset (* 0.33 r)
              dx (* offset eq/SQRT3_2)
              dy offset
              [a0 a1] (turns (+ (/ x width) (/ y height) (* 0.45 t))
                             (+ (- 1.0 (/ 1.0 x)) (/ 1.0 y) (* 0.25 t)))
              [b0 b1] (turns (+ (/ x width) (- 1.0 (/ y height)) (* 0.41 t))
                             (+ (/ 1.0 x) (/ 1.0 y) (* 0.23 t)))
              [c0 c1] (turns (+ (- 1.0 (/ x width)) (- 1.0 (/ y height)) (* 0.66 t))
                             (+ (/ 1.0 x) (/ 1.0 y) (* 0.37 t)))
              [d0 d1] (turns (+ (/ x width) (/ y height) (* 0.37 t))
                             (+ (/ 1.0 x) (- 1.0 (/ 1.0 y)) (* 0.27 t)))]
          (canvas/clockwise-arc ctx (gv/vec2 x y) (* 0.5 tm/SQRT2 r) a0 a1)
          (stroke-path ctx "rgba(0,0,0,0.66)")
          (canvas/clockwise-arc ctx (gv/vec2 x (- y dy)) r b0 b1)
          (fill-path ctx "rgba(240,0,240,0.4)")
          (canvas/clockwise-arc ctx (gv/vec2 (- x dx) (+ y (* 0.5 dy)))
                                r c0 c1)
          (fill-path ctx "rgba(0.0,240,240,0.4)")
          (canvas/clockwise-arc ctx (gv/vec2 (+ x dx) (+ y (* 0.5 dy)))
                                r d0 d1)
          (fill-path ctx "rgba(240,240,0,0.4)"))))
    ctx))

(defn page []
  (let [{:keys [canvas-state attributes]}
        (canvas/make-state {:width 900
                            :height 600
                            :draw #'draw}
                           {:width-pct 0.7})]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]
       [:div.contained
        [:div.centered.readable-width
         [:p "The arcs are a variation on Ben-Day dots using CMYK colors and
         arcs. Colors are formed from the visible portion of each arc instead of
         how much each dot overlaps. Double click to scale to fit the screen
         size."]]]])))

(sketch/definition othello
  {:created-at "2023-02-11"
   :type :canvas
   :tags #{}}
  (ctrl/mount (page)))

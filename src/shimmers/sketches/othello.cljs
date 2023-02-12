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
    (-> ctx
        (cv/clear 0 0 width height)
        (cv/color-fill "#000"))
    (dotimes [i cols]
      (dotimes [j rows]
        (let [a0 (* eq/TAU (eq/unit-cos (+  (/ j rows) (/ i cols) (* 0.45 t))))
              a1 (+ a0
                    (* eq/TAU
                       (eq/unit-sin (+ (/ 1.0 (inc i))
                                       (/ 1.0 (inc j))
                                       (* 0.25 t)))))]
          (cv/arc (cv/begin ctx)
                  (* (/ width cols) (+ i 0.5))
                  (* (/ height rows) (+ j 0.5))
                  (* (/ width cols) 0.45)
                  a0
                  a1
                  false)
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

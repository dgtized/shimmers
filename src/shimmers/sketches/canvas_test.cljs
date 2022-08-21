(ns shimmers.sketches.canvas-test
  (:require
   [goog.dom :as gdom]
   [helins.canvas :as cv]
   [reagent.core :as r]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: not quite updating the canvas size dynamically?
(defn toggle-size [canvas-state]
  (let [{:keys [width]} @canvas-state
        [w h] (if (= width 200) [300 300] [200 200])]
    (swap! canvas-state assoc :width w :height h)))

(defn update-box [state bounds]
  (let [{:keys [pos vel size]} state
        new-pos (tm/+ pos vel)]
    (cond (not (geometry/contains-box? bounds (rect/rect pos (tm/+ pos (gv/vec2 size size)))))
          (assoc state
                 :pos (g/unmap-point bounds (gv/vec2 0.5 0.5))
                 :vel vel)
          (geometry/contains-box? bounds (rect/rect new-pos (tm/+ new-pos (gv/vec2 size size))))
          (assoc state
                 :pos new-pos
                 :vel vel)
          :else
          (let [x (:x new-pos)
                face (if (or (< x (rect/left bounds))
                             (> (+ x size) (rect/right bounds)))
                       (gv/vec2 0 1)
                       (gv/vec2 1 0))
                new-vel (g/reflect vel face)]
            (assoc state
                   :pos pos
                   :vel new-vel)))))

;; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Optimizing_canvas#scaling_for_high_resolution_displays
(defn scale-dpi [canvas [width height]]
  (let [ctx (.getContext canvas "2d")
        dpr (gdom/getPixelRatio)]
    (set! (.-width canvas) (Math/floor (* dpr width)))
    (set! (.-height canvas) (Math/floor (* dpr height)))
    (set! (.-style.width canvas) (str width "px"))
    (set! (.-style.height canvas) (str height "px"))
    (.scale ctx dpr dpr)
    ctx))

;; TODO: Can the boxes bounce into the other canvas without sharing state?
;; also playing with neon effect from https://codepen.io/agar3s/pen/pJpoya
(defn draw-frame [id telemetry]
  (fn [_ canvas canvas-state]
    (let [{:keys [width height]} @canvas-state
          ctx (scale-dpi canvas [width height])
          margin 10
          size 50
          box-state (atom {:size size
                           :pos (gv/vec2 (dr/random-int margin (- width size margin))
                                         (dr/random-int margin (- height size margin)))
                           :vel (tm/normalize (gv/randvec2) 2.0)})]
      (cv/on-frame
       (fn [_]
         (let [{:keys [width height]} @canvas-state
               bounds (rect/rect 0 0 width height)
               {:keys [pos size] :as box-state'} (update-box @box-state bounds)
               [x y] pos]
           (swap! telemetry assoc id pos)
           (reset! box-state box-state')
           (-> ctx
               (cv/clear 0 0 width height)
               (cv/composite-op "lighter")
               (cv/line-join "round")
               (cv/shadow-blur "50")
               (cv/line-width 5)
               (cv/color-stroke (str "rgba(" 55 "," 240 "," 180 "," 5 ")"))
               (cv/rect-stroke x y size size))))))))

(defn page []
  (let [canvas-state (r/atom {:width 200 :height 200})
        telemetry (r/atom {})
        attributes
        {:class "canvas-frame"
         :style {:background "#000000"}}]
    (fn []
      [:div
       [:div.flexcols
        [:div
         [:h4 "Frame 1"]
         [canvas/canvas-frame attributes canvas-state
          (draw-frame :a telemetry)]]
        [:div
         [:h4 "Frame 2"]
         [canvas/canvas-frame attributes canvas-state
          (draw-frame :b telemetry)]]]
       [:div.explanation
        [:p.readable-width
         "Experimenting with an alternative Canvas renderer from Quil. As it can
   mount as a React component, it's easier to host multiple in a single
   sketch."]
        [:button {:on-click #(toggle-size canvas-state)} "Toggle Size"]
        (debug/display canvas-state)
        (debug/display telemetry)]])))

(sketch/definition canvas-test
  {:created-at "2021-11-18"
   :type :canvas}
  (ctrl/mount (page) "sketch-host"))

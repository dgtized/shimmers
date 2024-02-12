(ns shimmers.common.ui.canvas
  (:require
   [goog.dom :as dom]
   [reagent.core :as r]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.canvas-attributes :refer-macros [defattr]]
   [shimmers.math.equations :as eq]))

;; See also https://github.com/reagent-project/reagent-cookbook/tree/master/recipes/canvas-fills-div
(defn sizing-attributes [width height attributes]
  (-> attributes
      (merge {:width width :height height})
      (assoc-in [:style :width] (str width "px"))
      (assoc-in [:style :height] (str width "px"))))

;; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Optimizing_canvas#scaling_for_high_resolution_displays
(defn scale-dpi [ctx canvas [width height]]
  (let [dpr (dom/getPixelRatio)]
    (set! (.-width canvas) (Math/floor (* dpr width)))
    (set! (.-height canvas) (Math/floor (* dpr height)))
    (set! (.-style.width canvas) (str width "px"))
    (set! (.-style.height canvas) (str height "px"))
    (.scale ctx dpr dpr)
    ctx))

(defn round100 [n]
  (* 100 (int (/ n 100))))

(defn toggle-full-screen!
  [canvas-state
   {:keys [width-pct height-pct] :or {width-pct 1.0 height-pct 1.0}}]
  (let [{:keys [full-screen original width height]} @canvas-state]
    (if full-screen
      (let [{ow :width oh :height} original]
        ;; (println [ow oh (/ ow oh)])
        (swap! canvas-state assoc
               :width ow :height oh :full-screen false))
      (let [fw (round100 (* width-pct (.-innerWidth js/window)))
            fh (round100 (* height-pct (.-innerHeight js/window)))
            aspect (/ width height)
            [w h] (cond (and (>= height-pct 1.0) (> fw fh))
                        [fw (/ fw aspect)]
                        :else
                        [(* fh aspect) fh])]
        ;; (println [fw fh w h aspect])
        (swap! canvas-state assoc
               :width w :height h :full-screen true
               :original {:width width :height height})))))

;; TODO: how to make this lightweight enough to combine with devcards like visual tests?
;; As example, if I wanted a micro visual demo of contains-box?/contains-entity?
#_{:clj-kondo/ignore [:unused-binding]}
(defn animated-canvas [canvas-state attributes render-frame-fn]
  (let [!canvas (atom nil)
        cancel-animation (atom nil)]
    (r/create-class
     {:display-name "animated-canvas"
      :component-did-mount
      (fn []
        (reset! cancel-animation
                (render-frame-fn @!canvas canvas-state)))
      :component-will-unmount
      (fn [] (@cancel-animation))
      :reagent-render
      (fn [canvas-state attributes _]
        (let [{:keys [width height]} @canvas-state]
          [:canvas (assoc (sizing-attributes width height attributes)
                          :ref (fn [el] (reset! !canvas el)))]))})))

(defn canvas-frame [attrs canvas-state render-frame-fn]
  [animated-canvas canvas-state attrs render-frame-fn])

(defn on-animated-frame
  [{:keys [delay]} f]
  (let [cancel-id (volatile! nil)
        start (js/performance.now)
        periodic (volatile! (+ start delay))
        animation (gensym "on-animated-frame")]
    (letfn [(frame [timestamp]
              (when (and (> delay 0) (> timestamp @periodic))
                (vreset! periodic (+ timestamp delay))
                (println animation timestamp))
              (when (f timestamp)
                (vreset! cancel-id (js/requestAnimationFrame frame))))]
      (frame start))
    (fn cancel []
      (when (> delay 0)
        (println "cancel" animation))
      (js/cancelAnimationFrame @cancel-id)
      nil)))

(defn animate-frame
  "Lifecycle function for rendering a canvas.

  Expects `canvas-state` to have :width, :height, :initial, :update, and :draw
  specified. The latter three are for initializing, updating, and drawing the
  frame-state. Recommend specifying the function values with #' to ensure they can be
  updated at runtime."
  [canvas-el canvas-state]
  (let [measure-frames! (framerate/sampler)
        ctx (.getContext canvas-el "2d")
        setup (get @canvas-state :setup (fn [_] {}))
        frame-state (atom (setup @canvas-state))]
    (on-animated-frame
     {:delay 0}
     (fn [ms]
       (measure-frames! ms)
       (let [{:keys [width height] :as cv} @canvas-state
             screen-dims [width height]
             ctx (scale-dpi ctx canvas-el screen-dims)
             update-state (get cv :update (fn [_sd fs] fs))]
         (swap! frame-state update-state screen-dims ms)
         ((:draw cv) @frame-state ctx screen-dims ms))))))

(defn make-state [cv-config fs-config]
  (let [canvas-state (r/atom cv-config)
        toggle-fs (fn [] (toggle-full-screen! canvas-state fs-config))]
    {:attributes {:class "canvas-frame" :on-double-click toggle-fs}
     :canvas-state canvas-state}))

(defn clockwise-arc [ctx [x y] r t0 t1]
  (doto ctx
    .beginPath
    (.arc x y r t0 t1 nil)))

(defn circle [ctx {[x y] :p r :r}]
  (doto ctx
    .beginPath
    (.arc x y r 0 eq/TAU false)))

(defn move-to [ctx [x y]]
  (.moveTo ctx x y))

(defn line-to [ctx [x y]]
  (.lineTo ctx x y))

(defn stroke-rect [ctx x y width height]
  (.strokeRect ctx x y width height)
  ctx)

(defn stroke
  ([ctx] (.stroke ctx) ctx)
  ([ctx path] (.stroke ctx path) ctx))

(defn fill
  ([ctx] (.fill ctx) ctx)
  ([ctx fill-rule] (.fill ctx fill-rule) ctx)
  ([ctx path fill-rule] (.fill ctx path fill-rule) ctx))

(defn clear
  ([ctx width height] (clear ctx 0 0 width height))
  ([ctx x y width height]
   (.clearRect ctx x y width height)
   ctx))

(defattr alpha .-globalAlpha)
(defattr global-composite-op .-globalCompositeOperation)

(defattr fill-style .-fillStyle)

(defattr stroke-style .-strokeStyle)
(defattr line-cap .-lineCap)
(defattr line-dash-offset .-lineDashOffset)
(defattr line-join .-lineJoin)
(defattr line-width .-lineWidth)
(defattr miter-limit .-miterLimit)

(defattr shadow-style .-shadowColor)
(defattr shadow-blur .-shadowBlur)
(defattr shadow-x .-shadowOffsetX)
(defattr shadow-y .-shadowOffsetY)

(defattr font .-font)
(defattr font-kerning .-fontKerning)
(defattr text-direction .-direction)
(defattr text-align .-textAlign)
(defattr text-baseline .-textBaseline)

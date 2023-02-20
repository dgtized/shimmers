(ns shimmers.common.ui.canvas
  (:require
   [goog.dom :as dom]
   [reagent.core :as r]))

;; See also https://github.com/reagent-project/reagent-cookbook/tree/master/recipes/canvas-fills-div
(defn sizing-attributes [width height attributes]
  (-> attributes
      (merge {:width width :height height})
      (assoc-in [:style :width] (str width "px"))
      (assoc-in [:style :height] (str width "px"))))

;; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Optimizing_canvas#scaling_for_high_resolution_displays
(defn scale-dpi [canvas [width height]]
  (let [ctx (.getContext canvas "2d")
        dpr (dom/getPixelRatio)]
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
(defn animated-canvas [canvas-state attributes render-frame-fn]
  (let [!canvas (atom nil)
        cancel-animation (atom nil)]
    (r/create-class
     {:component-did-mount
      (fn []
        (reset! cancel-animation
                (render-frame-fn @!canvas canvas-state)))
      :component-will-unmount
      (fn [] (@cancel-animation))
      :reagent-render
      (fn []
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

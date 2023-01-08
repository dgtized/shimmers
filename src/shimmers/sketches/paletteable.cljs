(ns shimmers.sketches.paletteable
  (:require
   [goog.dom :as dom]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn scale-dpi [canvas [width height]]
  (let [ctx (.getContext canvas "2d")
        dpr (dom/getPixelRatio)]
    (set! (.-width canvas) (Math/floor (* dpr width)))
    (set! (.-height canvas) (Math/floor (* dpr height)))
    (set! (.-style.width canvas) (str width "px"))
    (set! (.-style.height canvas) (str height "px"))
    (.scale ctx dpr dpr)
    ctx))

(defn draw-canvas [!canvas width image]
  (when-let [canvas @!canvas]
    (let [sw (.-width image)
          sh (.-height image)
          ratio (/ sh (float sw))
          w width
          h (int (* w ratio))
          ctx (scale-dpi canvas [w h])]
      (.drawImage ctx image 0 0 sw sh 0 0 w h))))

(defn set-image-cb [!canvas _]
  (let [el (dom/getElement "still")]
    (when-let [file (first (.-files el))]
      (.then (js/createImageBitmap file)
             (fn [image]
               (draw-canvas !canvas 800 image))))))

(defn page []
  (let [!canvas (atom nil)]
    (fn []
      [:div
       [:div
        [:canvas
         {:class "canvas-frame"
          :ref #(reset! !canvas %)}]]
       [:div.contained
        [:label {:for "still"} "Select an image: "]
        [:input {:type "file"
                 :id "still"
                 :name "still"
                 :accept "image/png, image/jpg"
                 :on-change (partial set-image-cb !canvas)}]]])))

(sketch/definition paletteable
  {:created-at "2023-06-07"
   :type :canvas
   :tags #{:genuary2023}}
  (ctrl/mount (page) "sketch-host"))

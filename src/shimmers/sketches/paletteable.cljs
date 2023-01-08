(ns shimmers.sketches.paletteable
  (:require
   [goog.dom :as dom]
   [helins.canvas :as cv]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn set-image-cb [ui-state _]
  (let [el (dom/getElement "still")]
    (when-let [file (first (.-files el))]
      (.then (js/createImageBitmap file)
             (fn [image] (swap! ui-state assoc :image image))))))

(defn scale-dpi [canvas [width height]]
  (let [ctx (.getContext canvas "2d")
        dpr (dom/getPixelRatio)]
    (set! (.-width canvas) (Math/floor (* dpr width)))
    (set! (.-height canvas) (Math/floor (* dpr height)))
    (set! (.-style.width canvas) (str width "px"))
    (set! (.-style.height canvas) (str height "px"))
    (.scale ctx dpr dpr)
    ctx))

(defn draw-canvas [_ canvas state]
  (cv/on-frame
   (fn [_]
     (let [{:keys [width height]} @state
           ctx (scale-dpi canvas [width height])]
       (when-let [image (:image @state)]
         (let [sw (.-width image)
               sh (.-height image)]
           (.drawImage ctx image 0 0 sw sh 0 0 width height)))
       true))))

(defn page []
  (let [ui-state (ctrl/state {:width 800
                              :height 600
                              :image nil})]
    (fn []
      [:div
       [:div
        [canvas/canvas-frame
         {:class "canvas-frame"}
         ui-state
         draw-canvas]]
       [:div.contained
        [:label {:for "still"} "Select an image: "]
        [:input {:type "file"
                 :id "still"
                 :name "still"
                 :accept "image/png, image/jpg"
                 :on-change (partial set-image-cb ui-state)}]]])))

(sketch/definition paletteable
  {:created-at "2023-06-07"
   :type :canvas
   :tags #{:genuary2023}}
  (ctrl/mount (page) "sketch-host"))

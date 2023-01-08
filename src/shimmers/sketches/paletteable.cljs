(ns shimmers.sketches.paletteable
  (:require
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn draw-canvas [!canvas width image]
  (when-let [canvas @!canvas]
    (let [sw (.-width image)
          sh (.-height image)
          ratio (/ sh (float sw))
          w width
          h (int (* w ratio))
          ctx (canvas/scale-dpi canvas [w h])]
      (.drawImage ctx image 0 0 sw sh 0 0 w h))))

(defn set-image-cb [!upload !canvas width _]
  (when-let [upload @!upload]
    (when-let [file (first (.-files upload))]
      (.then (js/createImageBitmap file)
             (fn [image]
               (draw-canvas !canvas width image))))))

(defn page [width]
  (let [!canvas (atom nil)
        !upload (atom nil)]
    (fn []
      [:div
       [:canvas
        {:class "canvas-frame"
         :width width
         :ref #(reset! !canvas %)}]
       [:div.contained
        [:div.explanation
         [:label {:for "still"} "Select an image: "]
         [:input {:type "file"
                  :id "still"
                  :ref #(reset! !upload %)
                  :name "still"
                  :accept "image/png, image/jpg"
                  :on-change (partial set-image-cb !upload !canvas width)}]]]])))

(sketch/definition paletteable
  {:created-at "2023-06-07"
   :type :canvas
   :tags #{:genuary2023}}
  (ctrl/mount (page 800) "sketch-host"))

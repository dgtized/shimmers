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
          ctx (canvas/scale-dpi (.getContext canvas "2d") canvas [w h])]
      (.drawImage ctx image 0 0 sw sh 0 0 w h))))

(defn set-image-cb [!upload !canvas width _]
  (when-let [upload @!upload]
    (when-let [file (first (.-files upload))]
      (.then (js/createImageBitmap file)
             (fn [image]
               (draw-canvas !canvas width image))))))

;; See https://presumably.de/reagent-mysteries-part-3-manipulating-the-dom.html
;; and https://ericnormand.me/guide/reagent#form-3 for further discussion on
;; using the ref pattern.
(defn page []
  (let [width 800
        !canvas (atom nil)
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
                  :on-change (partial set-image-cb !upload !canvas width)}]
         [:p "Genuary 2023 Day 7: Sample a color palette from a movie/album cover"]
         [:p.readable-width "WIP: Was looking at combining this with the debug
         view prompt again, and allow file selection of a local image and then
         show debug output of palettes from that image. Considering just using
         basic k-means clustering using cos-similarity for palette discrimation,
         but still need to implement that."]]]])))

(sketch/definition paletteable
  {:created-at "2023-01-07"
   :type :canvas
   :tags #{:genuary2023}}
  (ctrl/mount (page)))

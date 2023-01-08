(ns shimmers.sketches.paletteable
  (:require
   [goog.dom :as dom]
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn set-image-cb [ui-state _]
  (let [el (dom/getElement "still")]
    (when-let [file (first (.-files el))]
      (let [reader (new js/FileReader)]
        (set! (.-onload reader)
              (fn [e]
                (let [result (-> e .-target .-result)]
                  (swap! ui-state assoc :image result))))
        (.readAsDataURL reader file)))))

(defn draw-canvas [_ canvas state])

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
                 :on-change (partial set-image-cb ui-state)}]
        [:div
         [:img {:id "preview"
                :src (if-let [image (:image @ui-state)]
                       image
                       "")
                :height 200}]]]])))

(sketch/definition paletteable
  {:created-at "2023-06-07"
   :type :canvas
   :tags #{:genuary2023}}
  (ctrl/mount (page) "sketch-host"))

(ns shimmers.sketches.paletteable
  (:require
   [goog.dom :as dom]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defonce ui-state (ctrl/state {:image nil}))

(defn set-image-cb [_]
  (let [el (dom/getElement "still")]
    (when-let [file (first (.-files el))]
      (let [reader (new js/FileReader)]
        (set! (.-onload reader)
              (fn [e]
                (let [result (-> e .-target .-result)]
                  (swap! ui-state assoc :image result))))
        (.readAsDataURL reader file)))))

(defn page []
  [:div
   [:div.contained
    [:label {:for "still"} "Select an image: "]
    [:input {:type "file"
             :id "still"
             :name "still"
             :accept "image/png, image/jpg"
             :on-change set-image-cb}]]
   [:div
    [:img {:id "preview"
           :src (if-let [image (:image @ui-state)]
                  image
                  "")
           :height 200}]]])

(sketch/definition paletteable
  {:created-at "2023-06-07"
   :type :canvas
   :tags #{:genuary2023}}
  (ctrl/mount page "sketch-host"))

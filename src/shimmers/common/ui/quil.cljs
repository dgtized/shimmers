(ns shimmers.common.ui.quil
  (:require
   [quil.core :as q :include-macros true]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

;; Amalgamation of:
;; https://github.com/quil/quil/issues/320#issuecomment-534859573
;; https://github.com/simon-katz/nomisdraw/blob/for-quil-api-request/src/cljs/nomisdraw/utils/nomis_quil_on_reagent.cljs

(defn sketch-component [sketch-args]
  [r/create-class
   {:component-did-mount
    (fn [component]
      (let [node (rdom/dom-node component)]
        (apply q/sketch (apply concat (assoc sketch-args :host node)))))
    :component-will-unmount
    (fn [component]
      (when-let [div-host (rdom/dom-node component)]
        (q/with-sketch (.-processing-obj div-host) (q/exit))))
    :render
    (fn []
      [:div.canvas-frame {:style {:position "relative"}}
       (when-let [performance-id (:performance-id sketch-args)]
         [:div.performance
          {:id performance-id
           :style {:position "absolute"
                   :color "#000"
                   :background "#ddd"
                   :opacity 0.66
                   :right 0
                   :top 0
                   :padding "0.1em 0.33em"
                   :z-index 100}}
          "00.0 fps"])])}])

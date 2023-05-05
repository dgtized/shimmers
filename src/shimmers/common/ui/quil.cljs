(ns shimmers.common.ui.quil
  (:require
   [quil.core :as q :include-macros true]
   [reagent.core :as r]))

;; Amalgamation of:
;; https://github.com/quil/quil/issues/320#issuecomment-534859573
;; https://github.com/simon-katz/nomisdraw/blob/for-quil-api-request/src/cljs/nomisdraw/utils/nomis_quil_on_reagent.cljs

(defn sketch-component [sketch-args]
  (let [!dom-node (atom nil)
        performance-id (name (gensym "performance_"))
        options (assoc sketch-args :performance-id performance-id)]
    (r/create-class
     {:display-name "quil-sketch-component"
      :component-did-mount
      (fn []
        (apply q/sketch (apply concat (assoc options :host @!dom-node))))
      :component-will-unmount
      (fn []
        (when-let [div-host @!dom-node]
          (q/with-sketch (.-processing-obj div-host) (q/exit))))
      :render
      (fn []
        [:div.canvas-frame {:style {:position "relative"}
                            :ref (fn [el] (reset! !dom-node el))}
         [:div.performance
          {:id performance-id
           :style {:position "absolute"
                   :color "#000"
                   :background "#ddd"
                   :opacity 0.66
                   :right 0
                   :top 0
                   :padding "0.1em 0.33em"
                   :z-index 100}}]])})))

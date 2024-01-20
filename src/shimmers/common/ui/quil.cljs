(ns shimmers.common.ui.quil
  (:require
   [quil.core :as q :include-macros true]
   [reagent.core :as r]))

(defn fps-overlay [performance-id]
  [:div.performance
   {:id performance-id
    :style {:position "absolute"
            :color "#000"
            :background "#ddd"
            :opacity 0.66
            :right 0
            :top 0
            :padding "0.1em 0.33em"
            :z-index 100}}])

(defn configure-fps-overlay [sketch-args]
  (let [overlay (:performance-id sketch-args)
        performance-id (if (= overlay :fps-overlay)
                         (name (gensym "performance_"))
                         "framerate")]
    (assoc sketch-args :performance-id performance-id)))

;; Amalgamation of:
;; https://github.com/quil/quil/issues/320#issuecomment-534859573
;; https://github.com/simon-katz/nomisdraw/blob/for-quil-api-request/src/cljs/nomisdraw/utils/nomis_quil_on_reagent.cljs

(defn sketch-component [sketch-args]
  (let [!dom-node (get sketch-args :dom-node (atom nil))
        {:keys [performance-id] :as options} (configure-fps-overlay sketch-args)]
    (r/create-class
     {:display-name "quil-sketch"
      :component-did-mount
      (fn []
        (apply q/sketch (apply concat (assoc options :host @!dom-node))))
      :component-will-unmount
      (fn []
        (when-let [div-host @!dom-node]
          (q/with-sketch (.-processing-obj div-host) (q/exit))))
      :render
      (fn [_sketch-args]
        [:div.canvas-frame {:style {:position "relative"}
                            :ref (fn [el] (reset! !dom-node el))}
         (when-not (= performance-id "framerate")
           [fps-overlay performance-id])])})))

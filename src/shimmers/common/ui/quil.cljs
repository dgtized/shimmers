(ns shimmers.common.ui.quil
  (:require
   [quil.core :as q :include-macros true]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

;; Amalgamation of:
;; https://github.com/quil/quil/issues/320#issuecomment-534859573
;; https://github.com/simon-katz/nomisdraw/blob/for-quil-api-request/src/cljs/nomisdraw/utils/nomis_quil_on_reagent.cljs

;; FIXME: will-unmount is not actually unmounting the corresponding component to
;; test, view reagent-quil-component, and then return to index and adjust `draw`
;; to print, and it will continue to run.
(defn sketch-component [sketch-args]
  [r/create-class
   {:component-did-mount
    (fn [component]
      (println "did-mount" component)
      (let [node (rdom/dom-node component)]
        (println "on-start " node)
        (apply q/sketch (apply concat (assoc sketch-args :host node)))))
    :component-will-unmount
    (fn [component]
      (println (str "will-unmount " component))
      (when-let [quil (rdom/dom-node component)]
        (q/with-sketch quil (q/exit))))
    :render
    (fn []
      (println "render ")
      [:div.canvas-frame])}])

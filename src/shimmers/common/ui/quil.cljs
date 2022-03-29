(ns shimmers.common.ui.quil
  (:require
   [quil.core :as q :include-macros true]
   [cljs.core.async :as a :include-macros true]
   [reagent.core :as r]))

;; Amalgamation of:
;; https://github.com/quil/quil/issues/320#issuecomment-534859573
;; https://github.com/simon-katz/nomisdraw/blob/for-quil-api-request/src/cljs/nomisdraw/utils/nomis_quil_on_reagent.cljs
(defn sketch-component [sketch-args]
  (let [[w h] (get sketch-args :size)
        host-id (get sketch-args :host "quil-host")
        canvas (keyword (str "canvas#" host-id))]
    [r/create-class
     {:component-did-mount
      (fn [_]
        (println (str "did-mount " host-id))
        (a/go
          (do (println "on-start")
              (apply q/sketch (apply concat (assoc sketch-args :host host-id))))))
      :component-will-unmount
      (fn [_]
        (println (str "will-unmount " host-id))
        (when-let [quil (q/get-sketch-by-id host-id)]
          (q/with-sketch quil (q/exit))))
      :render
      (fn []
        (println "render " canvas)
        [:div.canvas-frame [canvas {:width w :height h}]])}]))

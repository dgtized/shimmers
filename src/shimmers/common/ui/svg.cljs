(ns shimmers.common.ui.svg
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.sketch :as sketch]
   [shimmers.view.sketch :as view-sketch]))

(defn download-shortcut [id filename]
  [kb/kb-action "alt-s"
   (fn [] (svg-export/download id filename))])

(defn page
  [{:keys [sketch-id explanation]
    :as sketch-args}
   scene]
  (fn []
    (let [default-args {:scene-id "scene"}
          {:keys [scene-id] :as args} (merge default-args sketch-args)]
      [sketch/with-explanation
       [:div.canvas-frame
        [scene args]
        [download-shortcut scene-id (name sketch-id)]]
       [:p.center [view-sketch/generate sketch-id]]
       (when explanation
         [:div.readable-width
          [explanation sketch-args]])])))

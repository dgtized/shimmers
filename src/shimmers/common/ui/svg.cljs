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
  ([sketch-args scene] (page sketch-args scene nil))
  ([{:keys [sketch-id scene-id]
     :or {scene-id "scene"}
     :as sketch-args}
    scene explanation]
   (fn []
     [sketch/with-explanation
      [:div.canvas-frame [scene scene-id]]
      [download-shortcut scene-id (name sketch-id)]
      [:p.center [view-sketch/generate sketch-id]]
      (when explanation
        [:div.readable-width
         [explanation sketch-args]])])))

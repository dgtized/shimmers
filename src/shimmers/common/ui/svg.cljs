(ns shimmers.common.ui.svg
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg-export :as svg-export]))

(defn download-shortcut [id filename]
  [kb/kb-action "alt-s"
   (fn [] (svg-export/download id filename))])

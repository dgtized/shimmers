(ns shimmers.framerate
  (:require [goog.string :as gs]
            [goog.dom :as dom]))

;; TODO: move to quil middleware
(defn display [value]
  (let [rate (if (= value "")
               ""
               (gs/format "(framerate: %.1f)" value))]
    (dom/setTextContent (dom/getElement "framerate") rate)))

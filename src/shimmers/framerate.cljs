(ns shimmers.framerate
  (:require [goog.string :as gs]
            [goog.dom :as dom]))

(defn display [value]
  (let [rate (if (= value "")
               ""
               (gs/format "(framerate: %.1f)" value))]
    (dom/setTextContent (dom/getElement "framerate") rate)))

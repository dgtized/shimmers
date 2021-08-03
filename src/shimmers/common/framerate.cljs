(ns shimmers.common.framerate
  ;; see https://martinklepsch.org/posts/requiring-closure-namespaces.html for
  ;; weirdness around requiring goog.string.format to force it to create format
  ;; in goog.string. Requiring goog.string will not add format to the namespace,
  ;; but works until compiling with :optimization :advanced.
  (:require [goog.string.format]
            [goog.dom :as dom]
            [quil.core :as q]))

(defn display [node value]
  (let [rate (cond (= value "") ""
                   (= value 0) ""
                   :else (goog.string/format "%04.1f fps" value))]
    (dom/setTextContent node rate)))

(defn mode
  "Quil Middleware to update framerate"
  [options]
  (let [node (dom/getElement "framerate")
        draw (:draw options (fn [_]))
        timed-draw (fn [& args]
                     (apply draw args)
                     (display node (q/current-frame-rate)))]
    (when node
      (assoc options :draw timed-draw))))

(ns shimmers.common.framerate
  ;; see https://martinklepsch.org/posts/requiring-closure-namespaces.html for
  ;; weirdness around requiring goog.string.format to force it to create format
  ;; in goog.string. Requiring goog.string will not add format to the namespace,
  ;; but works until compiling with :optimization :advanced.
  (:require [goog.string.format]
            [goog.dom :as dom]
            [quil.core :as q]))

(defn display [value]
  (let [rate (if (= value "")
               ""
               (goog.string/format "%04.1f fps" value))]
    (dom/setTextContent (dom/getElement "framerate") rate)))

(defn mode
  "Quil Middleware to update framerate"
  [options]
  (let [draw (:draw options (fn [_]))
        timed-draw (fn [& args]
                     (apply draw args)
                     (display (q/current-frame-rate)))]
    (assoc options :draw timed-draw)))

(ns shimmers.common.framerate
  (:require
   [goog.dom :as dom]
   [quil.core :as q]
   [shimmers.common.string :as scs]))

(defn display [value]
  (let [node (dom/getElement "framerate")
        rate (cond (= value "") ""
                   (= value 0) ""
                   :else (scs/format "%04.1f fps" value))]
    (when node
      (dom/setTextContent node rate))))

(defn mode
  "Quil Middleware to update framerate"
  [options]
  (let [draw (:draw options (fn [_]))
        timed-draw (fn [& args]
                     (apply draw args)
                     (display (q/current-frame-rate)))]
    (assoc options :draw timed-draw)))

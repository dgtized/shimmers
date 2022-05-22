(ns shimmers.common.framerate
  (:require
   [goog.dom :as dom]
   [quil.core :as q]
   [shimmers.common.string :as scs]))

(defn display [host value]
  (let [rate (cond (= value "") ""
                   (= value 0) ""
                   :else (scs/cl-format "~1,2$ fps" value))]
    (when-let [node (dom/getElement host)]
      (dom/setTextContent node rate))))

(defn mode
  "Quil Middleware to update framerate"
  [options]
  (let [draw (:draw options (fn [_]))
        host (get options :performance-id "framerate")
        timed-draw (fn timed-draw [& args]
                     (apply draw args)
                     (display host (q/current-frame-rate)))]
    (assoc options :draw timed-draw)))

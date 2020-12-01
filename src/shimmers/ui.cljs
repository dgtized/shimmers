(ns shimmers.ui)

;; https://coderwall.com/p/s3j4va/google-analytics-tracking-in-clojurescript
;; Bypasses externs by grabbing gtag function from window directly and then
;; converting arguments to js array
(defn gtag [& args]
  (when js/gtag
    (.. (aget js/window "gtag")
        (apply nil (clj->js args)))))

(defn screen-view [sketch-name]
  (gtag "event" "screen_view" {:screen_name sketch-name}))

(defn cycle-next [lst current]
  (->> lst
       (into [])
       cycle
       (drop-while (fn [x] (not= current x)))
       (drop 1)
       first))

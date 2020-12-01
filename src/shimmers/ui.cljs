(ns shimmers.ui)

(defn screen-view [sketch-name]
  (.gtag js/window "event" "screen_view" {:screen_name sketch-name}))

(defn cycle-next [lst current]
  (->> lst
       (into [])
       cycle
       (drop-while (fn [x] (not= current x)))
       (drop 1)
       first))

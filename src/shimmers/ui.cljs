(ns shimmers.ui)

(defn cycle-next [lst current]
  (->> lst
       (into [])
       cycle
       (drop-while (fn [x] (not= current x)))
       (drop 1)
       first))

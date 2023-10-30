(ns shimmers.common.screen
  (:require [clojure.edn :as edn]))

(defn sizes []
  (into {}
        (for [size ["800x600"
                    "900x600"
                    "1024x768"
                    "1600x1200"]]
          [size size])))

(defn parse-size [screen]
  (->> screen
       (re-seq #"(\d+)x(\d+)")
       first
       rest
       (map edn/read-string)
       vec))

(comment (parse-size "800x600"))

(ns shimmers.common.palette
  (:require
   [clojure.string :as str]))

(defn url->colors [url]
  (-> url
      (str/split #"/")
      last
      (str/split #"-")))

(defn from-urls [urls]
  (->> urls
       (map url->colors)
       (map (partial map (partial str "#")))))

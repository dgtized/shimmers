(ns shimmers.macros.loader
  (:require [clojure.string :as str]))

(defn namespace-to-id [namespace]
  (keyword (last (str/split (str namespace) #"\."))))

(defmacro sketches-with-meta
  "Attaches :meta information for each sketch during compile time."
  [sketches]
  (into []
        (for [sketch sketches]
          `{:id (namespace-to-id (:ns (meta (var ~sketch))))
            :fn ~sketch
            :meta (meta (var ~sketch))})))

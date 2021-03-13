(ns shimmers.macros.loader
  (:require [clojure.string :as str]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]))

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

(defmacro all-sketches
  []
  `(list ~@(map (fn [ns] `(quote ~(ana-api/ns-resolve ns 'run-sketch)))
                (filter #(re-matches #"^shimmers.sketches.sp.*" (name %))
                        (ana-api/all-ns)))))

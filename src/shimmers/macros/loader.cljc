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
  `(remove nil?
           (list ~@(map (fn [ns]
                          (when (ana-api/find-ns ns)
                            `{:id (namespace-to-id (quote ~ns))
                              ;; :doc (:doc (quote ~(ana-api/find-ns ns)))
                              :fn ~(symbol (name ns) "run-sketch")
                              :meta (select-keys (quote ~(ana-api/ns-resolve ns 'run-sketch))
                                                 [:file :line])}))
                        (filter #(re-matches #"^shimmers.sketches.s.*" (name %))
                                (ana-api/all-ns))))))

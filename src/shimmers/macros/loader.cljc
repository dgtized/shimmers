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
          `(let [m# (meta (var ~sketch))]
             {:id (namespace-to-id (:ns m#))
              :fn ~sketch
              :file (:file m#)
              :line (:line m#)}))))

(defmacro all-sketches
  "Create sketch definitions from every namespace under shimmers.sketches"
  []
  `[~@(keep (fn [ns]
              ;; This *attempts* to handle if a sketch is required or not from
              ;; shimmers.core. However it appears that at compile time *all*
              ;; namespaces are known, but at runtime, the symbol creation
              ;; fails as the namespace isn't technically loaded.
              ;;
              ;; Need a mechanism for detecting if namespace loaded that works
              ;; at runtime & compile time.
              (when (and (ana-api/find-ns ns (ana-api/ns-resolve ns 'run-sketch)))
                `{:id (namespace-to-id (quote ~ns))
                  :doc (:doc (quote ~(ana-api/find-ns ns)))
                  :fn ~(symbol (name ns) "run-sketch")
                  :meta (select-keys (quote ~(ana-api/ns-resolve ns 'run-sketch))
                                     [:file :line])
                  }))
            (filter #(re-matches #"^shimmers.sketches.*" (name %))
                    (ana-api/all-ns)))])

(ns shimmers.ring-server
  (:require
   [ring.util.response :refer [resource-response content-type not-found]]))

;; the routes that we want to be resolved to index.html
(def index-routes [#"^/$" #"^/sketches"])

(defn handler [req]
  ;; (println "ring-handler: " (:uri req) (re-find #"^/sketches" (:uri req)))
  (or
   (when (some (fn [route] (re-find route (:uri req))) index-routes)
     (some-> (resource-response "index.html" {:root "public"})
             (content-type "text/html; charset=utf-8")))
   (not-found "Not found")))

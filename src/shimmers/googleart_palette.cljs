(ns shimmers.googleart-palette
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

;; Not working because CORS of localhost?
(defn fetch [id]
  (let [address (str "https://artsexperiments.withgoogle.com/artpalette/search/" id)]
    (go (let [response (<! (http/get address {:accept "*/*"}))]
          (pr response)))))

(comment
  (fetch "9f9fa3-aeae07-bcbbb7-cf8311"))

#!/usr/bin/env bb

;; Install babashka to local path with something like:
;; wget https://github.com/babashka/babashka/releases/download/v0.3.8/babashka-0.3.8-linux-amd64-static.tar.gz -O babashka.tgz &&
;;   tar -zxf babashka.tgz && mv -vf bb ~/usr/bin && rm -vf babashka.tgz

(require '[babashka.curl :as curl]
         '[clojure.string :refer [replace-first]]
         '[cheshire.core :as json])

(defn hexify [color]
  (apply str (cons "#" (map #(format "%02x" %) color))))

(defn extract-rgb [x]
  (let [elements (get x "palette")]
    (for [elem elements]
      (hexify (vec (map #(get elem %) ["r" "g" "b"]))))))

(defn fetch [id]
  (let [address (str "https://artsexperiments.withgoogle.com/artpalette/search/" id)
        response (curl/get address)]
    (if (= 200 (:status response))
      (map extract-rgb (json/parse-string (replace-first (:body response) ")]}',\n" "")))
      response)))

;; The colors were in the URL slug all along, no scraping necessary. This was
;; completely unnecessary other than learning how to use babashka, the palette
;; colors are the the key for lookup, but this gives whatever the PER picture
;; palette is? Maybe dominate colors clustered for that picture and interelated
;; with "nearby" pictures in the cluster?
(prn (fetch "9f9fa3-aeae07-bcbbb7-cf8311"))

;; So https://artsexperiments.withgoogle.com/artpalette/colors/9f9fa3-aeae07-bcbbb7-cf8311
;; gives the whole page and the color editor just changes the ids in the url to
;; pass off to search to look for images.


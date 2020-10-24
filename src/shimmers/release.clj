(ns shimmers.release
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn make-index [_]
  (println "[shimmers.release] Rewriting index.html -> shimmers.html")
  (let [input "resources/public/index.html"
        output "resources/public/shimmers.html"]
    (spit output (str/replace (slurp input) #"dev-main\.js" "release-main.js"))))

(ns shimmers.release
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]))

(defn revision-tag []
  (let [date (-> (sh "date" "--iso-8601=seconds" "-u")
                 :out
                 str/trimr
                 (str/replace #"\+00:00" "Z"))
        revision (-> (sh "git" "rev-parse" "HEAD")
                     :out
                     str/trimr)]
    (str "<span id=\"revision\"><code>"
         (subs revision 0 10)
         " "
         date
         "</code></span>")))

(defn make-index [_]
  (let [index "resources/public/index.html"
        revision (revision-tag)
        contents (slurp index)]
    (println (str "[shimmers.release] Rewriting index.html -> shimmers.html - " revision))
    (spit index
          (-> contents
              (str/replace-first #"dev-main\.js" "release-main.js")
              (str/replace-first "<span id=\"revision\"></span>" revision)))))

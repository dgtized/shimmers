(ns shimmers.release
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]])
  (:import java.text.SimpleDateFormat
           [java.util Calendar TimeZone]))

(defn timestamp-iso8601
  "Returns current ISO 8601 compliant date."
  []
  (let [sdf (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")]
    (.setTimeZone sdf (TimeZone/getTimeZone "UTC"))
    (.format sdf (.getTime (Calendar/getInstance)))))

(defn revision-tag []
  (let [revision (-> (sh "git" "rev-parse" "HEAD")
                     :out
                     str/trimr)]
    (str "<span id=\"revision\" title=\""
         (timestamp-iso8601)
         "\"><code>rev:"
         (subs revision 0 8)
         "</code></span>")))

(def base-href "https://dgtized.github.io/shimmers/")

(defn make-index [_]
  (let [index "resources/public/index.html"
        revision (revision-tag)
        contents (slurp index)]
    (println (str "[shimmers.release] Rewriting index.html - " revision))
    (spit index
          (-> contents
              (str/replace-first #"<base href=\".*\" />"
                                 (str "<base href=\"" base-href "\" />"))
              (str/replace-first #"dev-main\.js" "release-main.js")
              (str/replace-first "<span id=\"revision\"></span>" revision)))))

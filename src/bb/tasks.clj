(ns bb.tasks
  (:require [babashka.fs :as fs]
            [babashka.tasks :as bt]
            [clojure.edn :as edn]
            [clojure.java.shell :as shell :refer [sh]]
            [clojure.string :as str])
  (:import java.time.format.DateTimeFormatter
           java.time.Instant))

(defn timestamp-iso8601 []
  (let [fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss'Z'")]
    (.format fmt (.atZone (Instant/now) (java.time.ZoneId/of "Z")))))

(defn git-revision []
  (-> (sh "git" "rev-parse" "HEAD") :out str/trim))

(defn revision-span [timestamp sha]
  (str "<span id=\"revision\" title=\""
       timestamp
       "\"><code>rev:"
       (subs sha 0 8)
       "</code></span>"))

(defn release-file [{:keys [build-dir manifest release]
                     :or {manifest "manifest.edn"
                          release "release-main.js"}}]
  (let [manifest-file (str build-dir manifest)
        release-build (str build-dir release)
        manifest-db (edn/read-string (slurp manifest-file))]
    (fs/file-name (get manifest-db release-build))))

(defn build-static-site [& {:keys [build-dir from to release]}]
  (let [js-dir (str to "/js")
        release-base (fs/strip-ext (or release "release-main.js"))
        release-glob (str "/" (fs/file-name release-base) "*")]
    (fs/delete-tree to)
    (println "Creating" to "from" from "with javascript")
    (fs/copy-tree from to)
    (fs/create-dirs js-dir)
    (doseq [js (fs/glob build-dir (str "**" release-base "*"))]
      (fs/copy js js-dir))
    (bt/shell "bash" "-c" (str "ls -hs --format=single-column " js-dir release-glob))))

(defn rewrite-index [& {:keys [from to base-href] :as opts}]
  (let [revision (git-revision)
        timestamp (timestamp-iso8601)
        contents (slurp from)]
    (println "Rewriting" from "->" to)
    (println "  build:" timestamp "rev:" revision)
    (spit to
          (-> contents
              (str/replace-first #"<base href=\"\">"
                                 (str "<base href=\"" base-href "\">"))
              (str/replace-first #"cljs-out\/dev-main\.js"
                                 (str "js/" (release-file opts)))
              (str/replace-first "<span id=\"revision\"><code>rev:abcdef12</code></span>"
                                 (revision-span timestamp revision))))))

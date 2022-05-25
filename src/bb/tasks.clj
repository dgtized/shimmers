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

(defn release-file [build-dir]
  (let [manifest (edn/read-string (slurp (str build-dir "manifest.edn")))]
    (fs/file-name (get manifest (str build-dir "release-main.js")))))

(defn build-static-site [& {:keys [build-dir from to]}]
  (let [js-dir (str to "/js")]
    (fs/delete-tree to)
    (println "Creating" to "from" from "with javascript")
    (fs/copy-tree from to)
    (fs/create-dirs js-dir)
    (doseq [js (fs/glob build-dir "**release-main*")]
      (fs/copy js js-dir))
    (bt/shell "bash" "-c" (str "ls -hs --format=single-column " js-dir "/release-main*"))))

(defn rewrite-index [& {:keys [build-dir from to base-href]}]
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
                                 (str "js/" (release-file build-dir)))
              (str/replace-first "<span id=\"revision\"><code>rev:abcdef12</code></span>"
                                 (revision-span timestamp revision))))))

(defn github-publish [& {:keys [dir repo fake]}]
  (letfn [(xsh [& cmd]
            (println (apply str "+ " (interpose " " cmd)))
            (apply bt/shell {:dir dir} cmd))]
    (println (str "Publishing from within: " dir " -> " repo))
    (xsh "git init")
    (xsh "git checkout" "-b" "gh-pages")
    (xsh "git add index.html .gitignore js/* css/* shaders/*")
    (xsh "git commit -m \"Deploy to Github pages\"")
    ((if fake println xsh) (str "git push --force --quiet \"" repo "\" gh-pages:gh-pages"))))

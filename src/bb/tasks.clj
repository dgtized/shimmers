(ns tasks
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

(def cljs-output-dir "target/public/cljs-out/")

(defn release-file []
  (let [manifest (edn/read-string (slurp (str cljs-output-dir "manifest.edn")))]
    (fs/file-name (get manifest (str cljs-output-dir "release-main.js")))))

(defn build-static-site [from dir]
  (let [js-dir (str dir "/js")]
    (fs/delete-tree dir)
    (println "Creating" dir "from" from "with javascript")
    (fs/copy-tree from dir)
    (fs/create-dirs js-dir)
    (doseq [js (fs/glob cljs-output-dir "**release-main*")]
      (fs/copy js js-dir))
    (bt/shell "bash" "-c" (str "ls -hs --format=single-column " js-dir "/release-main*"))))

(defn rewrite-index [& {:keys [from to base-href]}]
  (let [revision (git-revision)
        timestamp (timestamp-iso8601)
        contents (slurp from)]
    (println "Rewriting" from "->" to)
    (println "  build:" timestamp "rev:" revision)
    (spit to
          (-> contents
              (str/replace-first #"<base href=\"\">$" (str "<base href=\"" base-href "\">"))
              (str/replace-first #"cljs-out\/dev-main\.js" (str "js/" (release-file)))
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

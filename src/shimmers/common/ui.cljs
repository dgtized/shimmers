(ns shimmers.common.ui
  (:require [clojure.string :as str]))

;; https://coderwall.com/p/s3j4va/google-analytics-tracking-in-clojurescript
;; Bypasses externs by grabbing gtag function from window directly and then
;; converting arguments to js array
(defn gtag [& args]
  (when js/gtag
    (.. (aget js/window "gtag")
        (apply nil (clj->js args)))))

(defn screen-view [sketch-name]
  (gtag "event" "screen_view"
        {"app_name" "shimmers"
         "screen_name" sketch-name}))

(defn code-link [sketch]
  (if-let [{:keys [file line]} sketch]
    {:filename (last (str/split file #"/"))
     :href
     (-> file
         ;; file can either be relative from src or an absolute path
         (str/replace-first #"^.*src/"
                            "https://github.com/dgtized/shimmers/blob/master/src/")
         (str "#L" line))}
    {:filename "" :href ""}))

(comment
  (require '[shimmers.sketches :as sketches])
  (map code-link (take 2 (sketches/all))))

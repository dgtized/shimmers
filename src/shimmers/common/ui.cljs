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
  (gtag "event" "screen_view" {"screen_name" sketch-name}))

(defn cycle-next [lst current]
  (->> lst
       (into [])
       cycle
       (drop-while (fn [x] (not= current x)))
       (drop 1)
       first))

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
  (require '[shimmers.macros.loader :as loader :include-macros true]
           '[shimmers.sketches.particles :as particles]
           '[shimmers.sketches.object-permanence :as object-permanence])
  (map code-link (loader/sketches-with-meta [particles/run-sketch object-permanence/run-sketch])))

(ns shimmers.view
  (:require [clojure.string :as str]
            [reitit.frontend.easy :as rfe]
            [shimmers.math.deterministic-random :as dr]))


;; Note that seed is required so that the path "changes", even though some
;; sketches are not using seed.
(defn sketch-link [method sketch-name]
  (method :shimmers.core/sketch-by-name
          {:name sketch-name}
          {:seed (dr/fresh-seed-value)}))

(defn list-sketches [sketches]
  (into [:ul]
        (for [sketch sketches
              :let [title (->> [(when-let [created-at (:created-at sketch)]
                                  (str created-at))
                                (when-let [tags (seq (:tags sketch))]
                                  (str "tags:" (str/join "," (map name tags))))]
                               (filter some?)
                               (str/join " "))]]
          [:li [:a {:href (sketch-link rfe/href (:id sketch))
                    :title title}
                (:id sketch)]])))

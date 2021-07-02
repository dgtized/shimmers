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

(defn sketch-title [sketch]
  (->> [(when-let [created-at (:created-at sketch)]
          (str created-at))
        (when-let [tags (seq (:tags sketch))]
          (str "tags:" (str/join "," (map name tags))))]
       (filter some?)
       (str/join " ")))

(defn list-sketches [sketches]
  (into [:ul]
        (for [sketch sketches]
          [:li [:a {:href (sketch-link rfe/href (:id sketch))
                    :title (sketch-title sketch)}
                (:id sketch)]])))

(defn selector [active]
  (let [pages {::sketch-list "Alphabetically"
               ::sketches-by-date "By Date"
               ::sketches-by-tag "By Tag"}]
    (->> (for [[page link-name] pages]
           [:a {:href (when-not (= page active) (rfe/href page))}
            link-name])
         (interpose [:span " | "])
         (into [:div.selector
                "Listing: "]))))

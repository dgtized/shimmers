(ns shimmers.view
  (:require [cljc.java-time.local-date :as ld]
            [clojure.set :as set]
            [clojure.string :as str]
            [reitit.frontend.easy :as rfe]
            [shimmers.view.sketch :as view-sketch]))

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
          [:li [:a {:href (view-sketch/sketch-link rfe/href (:id sketch))
                    :title (sketch-title sketch)}
                (:id sketch)]])))

(defn selector [active]
  (let [pages {::index-alphabetical "Alphabetically"
               ::sketches-by-date "By Date"
               ::sketches-by-tag "By Tag"}]
    (->> (for [[page link-name] pages]
           [:a {:href (when-not (= page active) (rfe/href page))}
            link-name])
         (interpose [:span " | "])
         (into [:div.selector
                "Listing: "]))))

;; FIXME: links are *always* fresh now since the seed is baked in
(defn index-alphabetical [sketches]
  (let [[sketches-an sketches-mz]
        (split-with (fn [{:keys [id]}] (re-find #"^[a-mA-M]" (name id)))
                    sketches)]
    [:section.sketch-list
     [:h1 (str "All Sketches (" (count sketches) ")")]
     [:p "A digital sketch-book of generative art, visual effects, computer
     animation, visualizations of algorithms, and whatever else struck my fancy to
     implement or explore. Many are complete, and some I periodically revisit
     and tweak. For those inspired by other's works or tutorials, I do my best
     to give attribution in the source code."]
     (selector ::index-alphabetical)
     [:div.sketch-columns
      [:div.column [:h3 "A-M"] (list-sketches sketches-an)]
      [:div.column [:h3 "N-Z"] (list-sketches sketches-mz)]]]))

(defn year-month [{:keys [created-at]}]
  [(ld/get-year created-at)
   (str/capitalize (str (ld/get-month created-at)))])

(defn sketches-by-date [sketches]
  (let [sketches-by-date (sort-by :created-at sketches)
        grouped-by-month (partition-by year-month sketches-by-date)]
    [:section.sketch-list
     (selector ::sketches-by-date)
     (for [sketches grouped-by-month
           :let [[year month] (year-month (first sketches))]]
       [:div {:key (str year month)}
        [:h3.date (str month " " year " (" (count sketches) ")")]
        (list-sketches sketches)])]))

(defn sketches-by-tag [sketches]
  (let [sketches (remove (fn [s] (empty? (:tags s))) sketches)
        tags (reduce (fn [acc {:keys [tags]}] (set/union acc tags))
                     #{}
                     sketches)]
    [:section.sketch-list
     (selector ::sketches-by-tag)
     (for [tag (sort-by name tags)
           :let [tagged-sketches (filter #(tag (:tags %)) sketches)]]
       [:div {:key (str tag)}
        [:h3.tag (str (str/capitalize (name tag))
                      " (" (count tagged-sketches) ")")]
        (list-sketches tagged-sketches)])]))

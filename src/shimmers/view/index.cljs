(ns shimmers.view.index
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [reagent.core :as r]
            [reitit.frontend.easy :as rfe]
            [shimmers.view.sketch :as view-sketch]))

(defn sketch-title [sketch]
  (->> [(when-let [created-at (:created-at sketch)]
          (subs (.toISOString (js/Date. created-at)) 0 10))
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

(defonce text-filter (r/atom ""))

(defn filter-sketches [sketches]
  (let [terms @text-filter]
    (if (empty? terms)
      [sketches ""]
      [(filter (fn [{:keys [id]}] (re-find (re-pattern terms) (name id))) sketches)
       terms])))

(defn update-terms [event]
  (let [term (-> event .-target .-value)]
    (when (or (empty? term)
              (try (re-pattern term)
                   (catch js/Object _ false)))
      (reset! text-filter term))))

(defn filtered-terms [sketches filtered terms]
  (when (seq terms)
    [:p "Found " (count filtered)
     " of " (count sketches)
     " sketches matching term \"" terms "\""]))

(defn selector [active]
  (let [pages {::by-alphabetical "Alphabetically"
               ::by-date "By Date"
               ::by-tag "By Tag"}
        search-input [:input {:type :search
                              :placeholder "search by name"
                              :value @text-filter
                              :on-input update-terms}]
        links (for [[page link-name] pages]
                [:a {:href (when-not (= page active) (rfe/href page))}
                 link-name])]
    (into [:div.selector
           search-input
           " Listing: "]
          (interpose [:span " | "] links))))

;; FIXME: links are *always* fresh now since the seed is baked in
(defn by-alphabetical [sketches]
  (let [[filtered terms] (filter-sketches sketches)]
    [:section.sketch-list
     [:h1 (str "All Sketches (" (count sketches) ")")]
     [:p "A digital sketch-book of generative art, visual effects, computer
     animation, visualizations of algorithms, and whatever else struck my fancy to
     implement or explore. Many are complete, and some I periodically revisit
     and tweak. For those inspired by other's works or tutorials, I do my best
     to give attribution in the source code."]
     (selector ::by-alphabetical)
     (filtered-terms sketches filtered terms)
     (if (< (count filtered) 20)
       [:div.sketch-columns
        [:div.column [:h3 "A-Z (" (count filtered) ")"] (list-sketches filtered)]]
       (let [[sketches-an sketches-mz]
             (split-with (fn [{:keys [id]}] (re-find #"^[a-mA-M]" (name id)))
                         filtered)]
         [:div.sketch-columns
          [:div.column [:h3 "A-M"] (list-sketches sketches-an)]
          [:div.column [:h3 "N-Z"] (list-sketches sketches-mz)]]))]))

;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
(defn year-month [{:keys [created-at]}]
  (let [date (js/Date. created-at)
        intl (js/Intl.DateTimeFormat. "en-US" #js{:month "long" :timeZone "UTC"})]
    [(.getUTCFullYear date) (.format intl date)]))

(defn by-date [sketches]
  (let [[filtered terms] (filter-sketches sketches)
        sketches-by-date (sort-by :created-at filtered)
        grouped-by-month (partition-by year-month sketches-by-date)]
    [:section.sketch-list
     (selector ::by-date)
     (filtered-terms sketches filtered terms)
     (for [sketches grouped-by-month
           :let [[year month] (year-month (first sketches))]]
       [:div {:key (str year month)}
        [:h3.date (str month " " year " (" (count sketches) ")")]
        (list-sketches sketches)])]))

(defn all-tags [sketches]
  (reduce (fn [acc {:keys [tags]}] (set/union acc tags))
          #{} sketches))

(defn by-tag [sketches]
  (let [tagged (remove (fn [s] (empty? (:tags s))) sketches)
        [filtered terms] (filter-sketches tagged)
        tags (all-tags filtered)]
    [:section.sketch-list
     (selector ::by-tag)
     (filtered-terms tagged filtered terms)
     (for [tag (sort-by name tags)
           :let [tagged-sketches (filter #(tag (:tags %)) filtered)]]
       [:div {:key (str tag)}
        [:h3.tag (str (str/capitalize (name tag))
                      " (" (count tagged-sketches) ")")]
        (list-sketches tagged-sketches)])]))

(ns shimmers.view.index
  (:require [cljc.java-time.local-date :as ld]
            [clojure.set :as set]
            [clojure.string :as str]
            [reagent.core :as r]
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
    [:p "Filtering out " (- (count sketches) (count filtered))
     " sketches with term \""
     terms "\""]))

(defn selector [active]
  (let [pages {::by-alphabetical "Alphabetically"
               ::by-date "By Date"
               ::by-tag "By Tag"}]
    (->> (for [[page link-name] pages]
           [:a {:href (when-not (= page active) (rfe/href page))}
            link-name])
         (interpose [:span " | "])
         (into [:div.selector
                [:input {:type :text :placeholder "search"
                         :on-input update-terms}]
                " Listing: "]))))

;; FIXME: links are *always* fresh now since the seed is baked in
(defn by-alphabetical [sketches]
  (let [[filtered terms] (filter-sketches sketches)
        [sketches-an sketches-mz]
        (split-with (fn [{:keys [id]}] (re-find #"^[a-mA-M]" (name id)))
                    filtered)]
    [:section.sketch-list
     [:h1 (str "All Sketches (" (count sketches) ")")]
     [:p "A digital sketch-book of generative art, visual effects, computer
     animation, visualizations of algorithms, and whatever else struck my fancy to
     implement or explore. Many are complete, and some I periodically revisit
     and tweak. For those inspired by other's works or tutorials, I do my best
     to give attribution in the source code."]
     (selector ::by-alphabetical)
     (filtered-terms sketches filtered terms)
     (cond (empty? filtered)
           [:div.sketch-columns>p "No matches"]
           (< (count filtered) 20)
           [:div.sketch-columns
            [:div.column [:h3 "A-Z (" (count filtered) ")"] (list-sketches filtered)]]
           :else
           [:div.sketch-columns
            [:div.column [:h3 "A-M"] (list-sketches sketches-an)]
            [:div.column [:h3 "N-Z"] (list-sketches sketches-mz)]])]))

(defn year-month [{:keys [created-at]}]
  [(ld/get-year created-at)
   (str/capitalize (str (ld/get-month created-at)))])

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

(defn by-tag [sketches]
  (let [tagged (remove (fn [s] (empty? (:tags s))) sketches)
        [filtered terms] (filter-sketches tagged)
        tags (reduce (fn [acc {:keys [tags]}] (set/union acc tags))
                     #{}
                     filtered)]
    [:section.sketch-list
     (selector ::by-tag)
     (filtered-terms tagged filtered terms)
     (for [tag (sort-by name tags)
           :let [tagged-sketches (filter #(tag (:tags %)) filtered)]]
       [:div {:key (str tag)}
        [:h3.tag (str (str/capitalize (name tag))
                      " (" (count tagged-sketches) ")")]
        (list-sketches tagged-sketches)])]))

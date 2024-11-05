(ns shimmers.common.edn
  "Adjusted formatting from `fipp.edn` for Clojure/EDN forms"
  (:require [fipp.ednize :refer [edn record->tagged]]
            [fipp.edn :refer [pretty-coll]]
            [fipp.visit :refer [visit visit*]]
            [fipp.engine :refer (pprint-document)]))

;; Variation on shimmers.common.ui.debug/fixed-width
(defn fixed-width
  "Format float `v` to `width` decimal places as long as it's not infinite."
  [v width]
  #?(:cljs
     (if (or (integer? v) (infinite? v))
       v
       (symbol (.toFixed v width)))
     :clj
     (if (or (integer? v)
             (= v Double/POSITIVE_INFINITY)
             (= v Double/NEGATIVE_INFINITY))
       v
       (symbol (format (str "%." width "f") v)))))

(defrecord EdnPrinter [symbols print-meta print-length print-level print-fixed-width]

  fipp.visit/IVisitor


  (visit-unknown [this x]
    (visit this (edn x)))


  (visit-nil [this]
    [:text "nil"])

  (visit-boolean [this x]
    [:text (str x)])

  (visit-string [this x]
    [:text (pr-str x)])

  (visit-character [this x]
    [:text (pr-str x)])

  (visit-symbol [this x]
    [:text (str x)])

  (visit-keyword [this x]
    [:text (str x)])

  (visit-number [this x]
    [:text (pr-str (fixed-width x print-fixed-width))])

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x)
      (pretty-coll this "(" x :line ")" visit)))

  (visit-vector [this x]
    (pretty-coll this "[" x :line "]" visit))

  (visit-map [this x]
    (pretty-coll this "{" x [:span "," :line] "}"
                 (fn [printer [k v]]
                   [:span (visit printer k) " " (visit printer v)])))

  (visit-set [this x]
    (pretty-coll this "#{" x :line "}" visit))

  (visit-tagged [this {:keys [tag form]}]
    [:group "#" (pr-str tag)
     (when (or (and print-meta (meta form))
               (not (coll? form)))
       " ")
     (visit this form)])


  (visit-meta [this m x]
    (if print-meta
      [:align [:span "^" (visit this m)] :line (visit* this x)]
      (visit* this x)))

  (visit-var [this x]
    [:text (str x)])

  (visit-pattern [this x]
    [:text (pr-str x)])

  (visit-record [this x]
    (visit this (record->tagged x))))

(defn pprint
  ([x] (pprint x {}))
  ([x options]
   (let [defaults {:symbols {}
                   :print-length *print-length*
                   :print-fixed-width 6
                   :print-level *print-level*
                   :print-meta *print-meta*}
         printer (map->EdnPrinter (merge defaults options))]
     (binding [*print-meta* false]
       (pprint-document (visit printer x) options)))))

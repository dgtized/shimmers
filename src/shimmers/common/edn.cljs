(ns shimmers.common.edn
  "Adjusted formatting from `fipp.edn` for Clojure/EDN forms"
  (:require
   [fipp.edn :refer [pretty-coll]]
   [fipp.ednize :refer [edn IEdn record->tagged]]
   [fipp.engine :refer [pprint-document]]
   [fipp.visit :refer [visit visit*]]
   [shimmers.common.string :refer [fixed-width]]
   [thi.ng.geom.types
    :refer [Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]]
   [thi.ng.geom.vector :refer [Vec2 Vec3]]))

;; identity operation for a map record that removes type info so tagged-literal
;; will not be called recursively with the original type. However this should
;; pickup extra keys if something else has been assoced in beyond the record keys.
;; TODO: worth including meta info?
(defn untyped [s]
  (zipmap (keys s) (vals s)))

;; Simplify IEdn output for pretty printing
(extend-protocol IEdn
  Circle2
  (-edn [s]
    (tagged-literal 'Circle2 (untyped s)))

  Line2
  (-edn [s]
    (tagged-literal 'Line2 (untyped s)))

  LineStrip2
  (-edn [s]
    (tagged-literal 'LineStrip2 (untyped s)))

  Polygon2
  (-edn [s]
    (tagged-literal 'Polygon2 (untyped s)))

  Rect2
  (-edn [s]
    (tagged-literal 'Rect2 (untyped s)))

  Triangle2
  (-edn [s]
    (tagged-literal 'Triangle2 (untyped s)))

  Vec2
  (-edn [s]
    (tagged-literal 'v2 (mapv (fn [v] (fixed-width v 2)) s)))

  Vec3
  (-edn [s]
    (tagged-literal 'v3 (mapv (fn [v] (fixed-width v 2)) s))))

(defrecord EdnPrinter [symbols print-meta print-length print-level print-fixed-width]

  fipp.visit/IVisitor


  (visit-unknown [this x]
    (visit this (edn x)))


  (visit-nil [_this]
    [:text "nil"])

  (visit-boolean [_this x]
    [:text (str x)])

  (visit-string [_this x]
    [:text (pr-str x)])

  (visit-character [_this x]
    [:text (pr-str x)])

  (visit-symbol [_this x]
    [:text (str x)])

  (visit-keyword [_this x]
    [:text (str x)])

  (visit-number [_this x]
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

  (visit-var [_this x]
    [:text (str x)])

  (visit-pattern [_this x]
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

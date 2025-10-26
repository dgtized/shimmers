(ns shimmers.common.string
  #?(:cljs (:require
            [cljs.pprint :as pp]
            [goog.string :as gstring]
            [goog.string.format])
     :clj (:require [clojure.pprint :as pp]))
  #?(:clj (:refer-clojure :exclude [format])))

;; see https://martinklepsch.org/posts/requiring-closure-namespaces.html for
;; weirdness around requiring goog.string.format to force it to create format
;; in goog.string. Requiring goog.string will not add format to the namespace,
;; but works until compiling with :optimization :advanced.

#?(:cljs
   (defn format [fmt & args]
     (apply gstring/format fmt args))
   :clj
   (def format clojure.core/format))

;; https://gigamonkeys.com/book/a-few-format-recipes.html
(defn cl-format [fmt & args]
  (apply pp/cl-format nil fmt args))

;; See https://github.com/brandonbloom/fipp/issues/83 for symbol hack, basically
;; it's forcing EdnPrinter to believe it's a symbol and not a float so it
;; doesn't wrap it in a string.
(defn fixed-width
  "Format float `v` to `width` decimal places as long as it's not infinite."
  ([v] (fixed-width v 2))
  ([v width]
   #?(:cljs
      (if (or (integer? v) (infinite? v))
        v
        (symbol (.toFixed v width)))
      :clj
      (if (or (integer? v)
              (= v Double/POSITIVE_INFINITY)
              (= v Double/NEGATIVE_INFINITY))
        v
        (symbol (format (str "%." width "f") v))))))

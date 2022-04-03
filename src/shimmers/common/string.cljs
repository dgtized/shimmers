(ns shimmers.common.string
  (:require
   [goog.string :as gstring]
   [goog.string.format]))

;; see https://martinklepsch.org/posts/requiring-closure-namespaces.html for
;; weirdness around requiring goog.string.format to force it to create format
;; in goog.string. Requiring goog.string will not add format to the namespace,
;; but works until compiling with :optimization :advanced.

(defn format [fmt & args]
  (apply gstring/format fmt args))


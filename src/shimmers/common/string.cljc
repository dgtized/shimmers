(ns shimmers.common.string
  #?(:cljs (:require
            [cljs.pprint :as pp]
            [goog.string :as gstring]
            [goog.string.format])
     :clj (:require [clojure.pprint :as pp])))

;; see https://martinklepsch.org/posts/requiring-closure-namespaces.html for
;; weirdness around requiring goog.string.format to force it to create format
;; in goog.string. Requiring goog.string will not add format to the namespace,
;; but works until compiling with :optimization :advanced.

#?(:cljs
   (defn format [fmt & args]
     (apply gstring/format fmt args)))


(defn cl-format [fmt & args]
  (apply pp/cl-format nil fmt args))

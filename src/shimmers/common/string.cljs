(ns shimmers.common.string
  (:require [goog.string :as gstring]
            [goog.string.format]))

(defn format [fmt & args]
  (apply gstring/format fmt args))


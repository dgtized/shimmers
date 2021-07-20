(ns shimmers.scratch.instaparse
  (:require [instaparse.core :as insta]))

;; basic example from https://github.com/Engelberg/instaparse
(def as-and-bs (insta/parser
                "S = AB*
                 AB = A B
                 A = 'a'+
                 B = 'b'+"))

(comment
  (as-and-bs "aaabbaaabbab"))

(ns shimmers.algorithm.chaikin
  "Translation of https://sighack.com/post/chaikin-curves"
  (:require [thi.ng.math.core :as tm]))

(defn cut [a b ratio]
  (let [ratio (if (> ratio 0.5) (- 1.0 ratio) ratio)]
    [(tm/mix a b ratio)
     (tm/mix b a ratio)]))

(defn chaikin [points ratio closed]
  (mapcat (fn [[a b]] (cut a b ratio))
          (partition 2 1 (if closed (conj points (first points))
                             points))))

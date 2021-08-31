(ns shimmers.algorithm.chaikin
  "Translation of https://sighack.com/post/chaikin-curves"
  (:require [shimmers.common.sequence :as cs]
            [thi.ng.math.core :as tm]))

(defn cut [a b ratio]
  (let [ratio (if (> ratio 0.5) (- 1.0 ratio) ratio)]
    [(tm/mix a b ratio)
     (tm/mix b a ratio)]))

(defn chaikin-open [points ratio]
  (let [r (mapcat (fn [[a b]] (cut a b ratio))
                  (partition 2 1 points))]
    (concat (take 1 points)
            (drop-last (rest r))
            (take-last 1 points))))

(defn chaikin-closed [points ratio]
  (mapcat (fn [[a b]] (cut a b ratio))
          (partition 2 1 (conj points (first points)))))

(defn chaikin [ratio closed iterations points]
  (let [approach (if closed chaikin-closed chaikin-open)]
    (cs/iterate-cycles iterations (fn [pts] (approach pts ratio)) points)))

(ns shimmers.automata.simplify
  (:require [meander.epsilon :as m]
            [meander.strategy.epsilon :as m*]))

;; Term Rewriting using Meander - https://github.com/noprompt/meander
;; References:
;; https://github.com/noprompt/meander/blob/epsilon/doc/cookbook.md
;; https://jimmyhmiller.github.io/meander-rewriting
;; https://jimmyhmiller.github.io/meander-practical
;; https://jimmyhmiller.github.io/building-meander-in-meander
(defn collapse-trivial-one-of
  "Recursively collapses single choice one-of's into that instruction"
  [instruction]
  ((-> (m*/rewrite
        [:one-of []] []
        [:one-of [?instruction ..1]] ?instruction)
       m*/attempt
       m*/bottom-up)
   instruction))

(defn collapse-commutative-groups
  "Collapse consecutive rotates, forward commands, and drop all but the last consecutive call to color and heading."
  [snippet]
  (let [[op arg] (first snippet)]
    (case op
      :rotate (if (vector? arg)
                snippet
                [[:rotate (reduce + (map second snippet))]]) ;; sum up consecutive rotations
      :forward (if (vector? arg)
                 snippet
                 [[:forward (reduce + (map second snippet))]]) ;; sum up consecutive forwards
      :color [(last snippet)] ;; last color wins
      :heading [(last snippet)] ;; last heading wins
      snippet)))

(defn simplify-program
  [program]
  (transduce
   (comp collapse-trivial-one-of
         (partition-by (fn [val] [(first val) (vector? (second val))]))
         (mapcat collapse-commutative-groups))
   conj program))

(defn expand-possible-instructions
  "For the purposes of detecting if a program does something meaningful before halting."
  [program]
  (mapcat (fn [[op argument]]
            (if (= op :one-of)
              ;; TODO: sort so halt instructions are last
              (expand-possible-instructions argument)
              [[op argument]]))
          program))

(defn accept-program?
  "Only accept programs that have a forward instruction before halting."
  [program]
  (let [expanded (map first (expand-possible-instructions program))
        up-to-halt (take-while #(not= % :halt) expanded)]
    (boolean (some #{:forward} up-to-halt))))

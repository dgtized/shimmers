(ns shimmers.automata.simplify
  (:require [cljs.core.match :refer-macros [match]]
            [shimmers.common.sequence :as cs]))

(defn expand-possible-instructions
  "For the purposes of detecting if a program does something meaningful before halting."
  [program]
  (mapcat (fn [[op argument]]
            (if (= op :one-of)
              ;; TODO: sort so halt instructions are last
              (expand-possible-instructions argument)
              [[op argument]]))
          program))

(defn collapse-trivial-one-of
  "Recursively collapses single choice one-of's into that instruction"
  [[op argument]]
  (if (= op :one-of)
    (let [options (mapcat collapse-trivial-one-of argument)]
      (if (> (count options) 1)
        [[:one-of options]]
        options))
    [[op argument]]))

(defn collapse-commutative-groups
  "Collapse consecutive rotates, forward commands, and drop all but the last consecutive call to color and heading."
  [snippet]
  (match (first snippet)
    ;; sum up consecutive rotations/forwards
    [(op :guard #{:rotate :forward}) arg]
    (if (vector? arg) snippet
        [[op (reduce + (map second snippet))]])
    ;; last color/heading wins
    [(:or :color :heading) _]
    [(last snippet)]
    ;; reduce to a single fork call
    [:fork _]
    [[:fork 0]]
    :else snippet))

(defn trim-after-halt
  "Trim programs to end on a halt if no goto instruction occurs prior."
  [program]
  (let [before-halt (cs/take-until (fn [[op _]] (= op :halt)) program)]
    (if (some #{:goto} (map first (expand-possible-instructions before-halt)))
      program
      before-halt)))

(defn simplify-program
  [program]
  (->> program
       (transduce
        (comp (mapcat collapse-trivial-one-of)
              (partition-by (fn [val] [(first val) (vector? (second val))]))
              (mapcat collapse-commutative-groups))
        conj)
       trim-after-halt))

(defn accept-program?
  "Only accept programs that have a forward instruction before halting, and both a
  rotate and forward command."
  [program]
  (let [expanded (map first (expand-possible-instructions program))
        up-to-halt (take-while #(not= % :halt) expanded)]
    (boolean
     (and (some #{:forward} up-to-halt)
          (some #{:rotate} expanded)
          (some #{:forward} expanded)))))

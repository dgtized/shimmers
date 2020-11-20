(ns shimmers.automata.simplify)

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
   (comp (mapcat collapse-trivial-one-of)
         (partition-by (fn [val] [(first val) (vector? (second val))]))
         (mapcat collapse-commutative-groups))
   conj program))

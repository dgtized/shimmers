(ns shimmers.algorithm.markov)

;; inspired by https://jackrusher.com/journal/markov-creativity.html
(defn learn
  ([dataset prefix-len suffix-len]
   (learn dataset prefix-len suffix-len identity {}))
  ([dataset prefix-len suffix-len reconstitute model]
   (->> dataset
        (partition (+ prefix-len suffix-len) 1)
        (reduce (fn [m example]
                  (let [[prefix suffix] (split-at prefix-len example)]
                    (update m (reconstitute prefix)
                            (fnil #(conj % (reconstitute suffix)) []))))
                model))))

(defn predict
  ([model] (predict model (-> model keys rand-nth)))
  ([model start]
   (if-let [transitions (get model start)]
     (let [choice (rand-nth transitions)]
       (lazy-cat (take 1 start) (predict model (concat (drop 1 start) choice))))
     start)))

(defn combine [a b]
  (merge-with concat a b))

(comment
  (def example "the quick brown fox jumped over the lazy dog")
  (learn example 2 1)
  (learn example 2 2 (partial apply str) {})

  (apply str (take 100 (predict (learn example 3 1) (seq "the"))))
  (apply str (take 100 (predict (learn example 2 1))))

  (combine (learn "the quick" 1 1) (learn "the brown" 1 1)))

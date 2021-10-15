(ns shimmers.automata.memory
  (:require [shimmers.math.probability :as p]))

(defrecord Allocation [id base size])

(defn overlaps? [allocations position]
  (some (fn [{:keys [base size] :as alloc}]
          (when (and (<= base position)
                     (< position (+ base size)))
            alloc))
        allocations))

(defn next-free [pages allocations position]
  (if-let [{:keys [base size]} (overlaps? allocations position)]
    (recur pages allocations (mod (+ base size) pages))
    (mod position pages)))

(defn next-bounds [pages allocations position]
  (let [after (->> allocations
                   (sort-by :base)
                   (drop-while (fn [{:keys [base size]}]
                                 (<= (+ base size) position))))]
    (if (seq after)
      (let [{:keys [base]} (first after)]
        (if (< base position)
          (next-bounds pages (rest after) position)
          base))
      pages)))

(defn allocate [id pages allocations size start]
  (let [base (next-free pages allocations start)
        bounds (next-bounds pages allocations base)
        extent (- bounds base)]
    (cond (zero? size)
          nil
          (<= size extent)
          [(->Allocation id base size)]
          :else
          (let [alloc (->Allocation id base extent)]
            (conj (allocate id pages (conj allocations alloc) (- size extent)
                            (mod (+ base extent) pages))
                  alloc)))))

(defn allocs-by-id [identifier]
  (fn [{:keys [id]}] (= id identifier)))

(defn allocation-ids [allocations]
  (dedupe (sort (map :id allocations))))

(defn guess-start [pages allocations]
  (if-let [{:keys [base size]} (last allocations)]
    (mod (+ base size) pages)
    0))

(defn malloc [{:keys [pages free next-id allocations] :as state} size]
  (if (> size free)
    state ;; allocation failed
    (let [start (guess-start pages allocations)
          allocs (allocate next-id pages allocations size start)]
      (-> state
          (update :free - size)
          (update :next-id inc)
          (update :allocations concat allocs)))))

(defn free [{:keys [allocations] :as state} free-id]
  (let [to-free (filter (allocs-by-id free-id) allocations)]
    (-> state
        (update :free + (reduce + (map :size to-free)))
        (update :allocations (partial remove (allocs-by-id free-id))))))

;; Take most fragmented allocation (ie group allocations by id), and combine 2+ groups into a single element
;; add concept of allocation chunk ordering?
(defn defrag [{:keys [pages free allocations] :as state}]
  (let [allocs (->> allocations
                    (group-by :id)
                    vals
                    (filter (fn [s] (> (count s) 1)))
                    (p/weighted-by count)
                    (sort-by :size))]
    (if (< (* 1.5 (reduce + (take 2 (map :size allocs))))
           free)
      (let [id (:id (first allocs))
            start (rand-int pages) ;; throw a dart and scan forward for next free
            ;; scan for largest free space?
            base (next-free pages allocations start)
            bounds (next-bounds pages allocations base)
            extent (- bounds base)
            allocs-with-sum (map vector allocs (reductions + (map :size allocs)))
            combine (take-while (fn [[_ sum]] (< sum extent)) allocs-with-sum)]
        (if (seq combine)
          (assoc state :allocations (conj (remove (set (map first combine)) allocations)
                                          (->Allocation id base (second (last combine)))))
          state))
      state)))

(comment (defrag {:free 6 :pages 10 :allocations [(->Allocation 1 0 1)
                                                  (->Allocation 1 2 1)
                                                  (->Allocation 1 4 1)
                                                  (->Allocation 1 5 1)]}))

(let [allocs [{:size 1} {:size 2} {:size 3} {:size 4}]]
  (take-while (fn [[_ sum]] (< sum 5))
              (map vector allocs (reductions + (map :size allocs)))))

(defn initialize [pages]
  {:pages pages
   :free pages
   :next-id 1
   :allocations []})

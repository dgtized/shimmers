(ns shimmers.automata.memory)

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


(comment (next-free 8 [(->Allocation 0 2 2)] 0)
         (next-free 8 [(->Allocation 0 2 2)] 2)
         (next-free 8 [(->Allocation 0 2 2)] 4)
         (next-free 8 [(->Allocation 0 2 2)] 5)
         (next-free 8 [(->Allocation 0 2 2)] 8)
         (next-bounds 8 [(->Allocation 0 2 2)] 0)
         (next-bounds 8 [(->Allocation 0 2 2) (->Allocation 0 4 1)] 3)
         (next-bounds 8 [(->Allocation 0 2 2)] 5)
         )

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

(comment
  (allocate 2 8 [(->Allocation 1 4 2)] 2 0)
  (allocate 2 8 [(->Allocation 1 4 2)] 2 2)
  (allocate 2 8 [(->Allocation 1 4 2)] 3 2)
  (allocate 2 8 [(->Allocation 1 4 2)] 4 4)
  (allocate 2 8 [(->Allocation 1 4 2)] 2 6)
  (allocate 2 8 [(->Allocation 1 4 2)] 2 8)
  (allocate 2 9 [(->Allocation 1 4 1) (->Allocation 1 6 1)] 6 0)
  (allocate 2 8 [(->Allocation 1 4 1) (->Allocation 1 6 1)] 5 1)
  )

(defn allocs-by-id [identifier]
  (fn [{:keys [id]}] (= id identifier)))

(defn malloc [{:keys [pages free next-id allocations] :as state} size]
  (if (> size free)
    state ;; allocation failed
    (let [last-alloc (last allocations)
          start (if-let [{:keys [base size]} last-alloc]
                  (mod (+ base size) pages)
                  0)
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

(defn initialize [pages]
  {:pages pages
   :free pages
   :next-id 1
   :allocations []})

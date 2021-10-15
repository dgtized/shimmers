(ns shimmers.sketches.memory-allocation
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.math.probability :as p]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

;; 4k blocks 2**24
;; 4k bytes per block 2**12

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

(defn allocs-by-id [identifier]
  (fn [{:keys [id]}] (= id identifier)))

(defn free [{:keys [allocations] :as state} free-id]
  (let [to-free (filter (allocs-by-id free-id) allocations)]
    (-> state
        (update :free + (reduce + (map :size to-free)))
        (update :allocations (partial remove (allocs-by-id free-id))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [pages (Math/pow 2 12)]
    {:pages pages
     :free pages
     :next-id 1
     :allocations []}))

(defn update-state [{:keys [allocations] :as state}]
  (cond (p/chance 0.5)
        state
        (and (not-empty allocations) (p/chance 0.3))
        (free state (rand-nth (dedupe (sort (map :id allocations)))))
        :else
        (malloc state (int (tm/random 4 128)))))

(def phi (/ (+ 1 (Math/sqrt 5)) 2))
(defn color [id]
  [(mod (* id phi) 1.0) 0.5 0.5 1.0])

(defn draw [{:keys [pages allocations]}]
  (q/background 1.0)
  (q/no-fill)
  (let [aspect (/ (q/width) (q/height))
        cols (int (Math/sqrt pages))
        w (/ (/ (q/width) cols) aspect)
        h (/ (q/height) (/ pages cols))]
    (doseq [y (range 0 (/ pages cols))]
      (doseq [x (range 0 cols 1)]
        (q/rect (* x w) (* y h) w h)))

    (doseq [{:keys [id base size]} allocations]
      (q/fill (color id))
      (dotimes [i size]
        (let [offset (+ base i)
              x (rem offset cols)
              y (quot offset cols)]
          (q/rect (* x w) (* y h) w h))))))

(sketch/defquil memory-allocation
  :created-at "2021-10-14"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

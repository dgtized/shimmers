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

(defn after [{:keys [base size]}]
  (+ base size))

(defn allocate [id pages allocations size]
  (let [base (after (last allocations))]
    (if (< (+ base size) pages)
      [(->Allocation id base size)]
      (let [extent (- pages base)]
        [(->Allocation id base extent)
         (->Allocation id 0 extent)]))))

(defn malloc [{:keys [pages free next-id allocations] :as state} size]
  (if (> size free)
    state ;; allocation failed
    (let [allocs (allocate next-id pages allocations size)]
      (-> state
          (update :free - size)
          (update :next-id inc)
          (update :allocations concat allocs)))))

;; (defn free [s id])

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [pages (Math/pow 2 12)]
    {:pages pages
     :free pages
     :next-id 1
     :allocations []}))

(defn update-state [state]
  (if (p/chance 0.9)
    state
    (malloc state (int (tm/random 1 64)))))

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

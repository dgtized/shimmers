(ns shimmers.sketches.kd-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defrecord Node [location axis lesser greater])

(defn children [{:keys [lesser greater]}]
  [lesser greater])

(defn kd-tree [points k depth]
  (when (seq points)
    (let [axis (mod depth k)
          sorted (sort-by #(nth % axis) points)
          half (int (/ (count sorted) 2))
          median (nth sorted half)]
      (->Node median
              axis
              (kd-tree (take half sorted) k (inc depth))
              (kd-tree (drop (inc half) sorted) k (inc depth))))))

;; TODO: implement nearest neighbors of radius and show points from mouse press position
;; pausing for now as thi.ng/geom has a quadtree library I can use.
(defn nearest-neighbor
  [tree point radius]
  )

(defn example []
  (map (fn [[x y]] [(* x 40) (- 400 (* y 40))])
       [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]]))

(defn random-example [n]
  (repeatedly n (fn []
                  [(int (q/random (q/width)))
                   (int (q/random (q/height)))])))

(defn init-state []
  {:source (random-example 96)
   :points []})

(defn setup []
  (q/frame-rate 3)
  (init-state))

(defn update-state [{:keys [points source] :as state}]
  (if (= (count points) (count source))
    (init-state)
    (assoc state
           :points (conj points (first source))
           :source (rest source))))

(defn draw-tree [tree bounds]
  (when (record? tree)
    (let [{:keys [axis location lesser greater]} tree
          [x y] location
          [lower upper] (nth bounds axis)
          next-axis (mod (inc axis) 2)
          [nlower nupper] (nth bounds next-axis)
          axis-bound (nth location axis)]
      (if (= axis 0)
        (do (q/stroke "red")
            (q/line x lower x upper))
        (do (q/stroke "blue")
            (q/line lower y upper y)))
      (draw-tree lesser (assoc bounds next-axis [nlower axis-bound]))
      (draw-tree greater (assoc bounds next-axis [axis-bound nupper])))))

(defn draw [{:keys [points] :as state}]
  (q/background "white")
  (q/stroke "black")
  ;; (q/fill "black")
  ;; (q/text-size 6)
  (let [tree (kd-tree points 2 0)]
    (doseq [point points]
      (q/stroke-weight 4)
      (apply q/point point)
      ;; (q/stroke-weight 0.5)
      ;; (apply q/text (str point) point)
      )
    (q/stroke "grey")
    (q/stroke-weight 0.5)
    (draw-tree tree [[0 (q/width)] [0 (q/height)]])))

(defn ^:export run-sketch []
  (q/defsketch show-kd-tree
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))



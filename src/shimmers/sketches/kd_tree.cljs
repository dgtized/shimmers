(ns shimmers.sketches.kd-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

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

(defn example []
  (map (fn [[x y]] [(* x 40) (- 400 (* y 40))])
       [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]]))

(defn random-example [n]
  (repeatedly n (fn []
                  [(int (q/random (q/width)))
                   (int (q/random (q/height)))])))

(defn setup []
  {:points (random-example 64)})

(defn draw-tree [tree bounds]
  (doseq [{:keys [axis location]}
          (->> tree
               (tree-seq record? children)
               (keep identity))
          :let [[x y] location
                [bound-lower bound-upper] (nth bounds axis)]]
    (cond (= axis 0) ;; x axis
          (q/line x bound-lower x bound-upper)
          :else
          (q/line bound-lower y bound-upper y))))

(defn draw [{:keys [points] :as state}]
  (q/background "white")
  (q/stroke-weight 2)
  (q/stroke "black")
  (let [tree (kd-tree points 2 0)]
    (doseq [point points]
      (apply q/point point))
    (q/stroke "grey")
    (q/stroke-weight 0.5)
    (draw-tree tree [[0 (q/width)] [0 (q/height)]])))

(defn ^:export run-sketch []
  (q/defsketch show-kd-tree
    :host "quil-host"
    :size [400 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))



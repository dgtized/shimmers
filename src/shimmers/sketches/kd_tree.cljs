(ns shimmers.sketches.kd-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defrecord node [location axis lesser greater])

(defn kd-tree [points k depth]
  (when (seq points)
    (let [axis (mod depth k)
          sorted (sort-by #(nth % axis) points)
          half (int (/ (count sorted) 2))
          median (nth sorted half)]
      (->node median
              axis
              (kd-tree (take half sorted) k (inc depth))
              (kd-tree (drop (inc half) sorted) k (inc depth))))))

(defn setup []
  {:points (repeatedly 16 (fn []
                            [(q/random (q/width))
                             (q/random (q/height))]))})

(defn draw-tree [tree]
  (doseq [{:keys [axis location]}
          (->> tree
               (tree-seq record? (fn [node] [(:lesser node) (:greater node)]))
               (keep identity))
          :let [[x y] location]]
    (cond (= axis 0) ;; x axis
          (q/line x 0 x (q/height))
          :else
          (q/line 0 y (q/width) y))))

(defn draw [{:keys [points] :as state}]
  (q/background "white")
  (q/stroke-weight 2)
  (q/stroke "black")
  (let [tree (kd-tree points 2 0)]
    (doseq [point points]
      (apply q/point point))
    (q/stroke "grey")
    (q/stroke-weight 0.5)
    (draw-tree tree)))

(defn ^:export run-sketch []
  (q/defsketch show-kd-tree
    :host "quil-host"
    :size [400 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))



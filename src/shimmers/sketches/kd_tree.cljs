(ns shimmers.sketches.kd-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]))

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
;; (defn nearest-neighbor
;;   [tree point radius]
;;   )

(defn example []
  (map (fn [[x y]] [(* x 40) (- 800 (* y 40))])
       [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]]))

(defn init-state []
  {:source (->> (geometry/generate-points 96 dr/random)
                (mapv cq/rel-vec)
                (map (partial map int))
                (mapv gv/vec2))
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

(defn draw [{:keys [points]}]
  (q/background "white")
  (q/stroke "black")
  (q/fill "black")
  (q/text-size 7)
  (let [tree (kd-tree points 2 0)]
    (doseq [[x y] points]
      (q/stroke-weight 4)
      (q/point x y)
      (q/stroke-weight 0.2)
      ;; (q/text (str [x y]) (+ x 5) (- y 5))
      )
    (q/stroke "grey")
    (q/stroke-weight 0.5)
    (draw-tree tree [[0 (q/width)] [0 (q/height)]])))

(sketch/defquil kd-tree-sketch
  :created-at "2020-11-28"
  :tags #{:datastructures}
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])



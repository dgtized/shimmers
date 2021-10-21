(ns shimmers.sketches.k-means
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn magnitude [v]
  (Math/sqrt (reduce + (map * v v))))

;; https://en.wikipedia.org/wiki/Cosine_similarity
(defn cos-similarity [v1 v2]
  (let [mag (* (magnitude v1) (magnitude v2))]
    (if (> mag 0)
      (/ (reduce + (map * v1 v2))
         mag)
      0)))

(defn draw-shape [{:keys [position color theta] :or {theta 0}}]
  (apply q/fill color)
  (-> (gt/triangle2 [0 0] [0 (q/random 13 21)] [(q/random 13 21) 0])
      (g/translate (cq/rel-pos position))
      (geometry/rotate-around-centroid theta)
      cq/draw-polygon))

(defn make-shape []
  {:position (gv/vec2 (rand) (rand))
   :color [(rand-nth (range 0 1 0.12))
           (q/random 0.3 0.8)
           (q/random 0.3 0.8)
           0.04]})

(defn grouping-vector [{:keys [color position]}]
  (let [v (concat (map * color (repeat 3)) position)
        m (magnitude v)]
    (mapv (fn [x] (/ x m)) v)))

(defn color-add [c1 c2]
  (map + c1 c2))

(defn pr-dbg [v]
  (pr v)
  v)

(defn seed-cluster [shapes n]
  (let [colors (into {} (map-indexed (fn [i s] {i (grouping-vector s)})
                                     (take n (shuffle shapes))))]
    (for [shape shapes]
      (assoc shape
             :cluster
             (apply max-key
                    (fn [cluster] (cos-similarity (grouping-vector shape) (get colors cluster)))
                    (keys colors))))))

;; Add "attractor" points that can't move or are outside bounds?
;; Generate initial triangles by sampling from video or an image?
(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsl 1.0)
  {:shapes (seed-cluster (repeatedly 256 make-shape) 12)})

(defn cluster->centroid
  [cluster-shapes]
  (->> cluster-shapes
       (map grouping-vector)
       (reduce color-add)
       (mapv (fn [x] (/ x (inc (count cluster-shapes)))))))

(defn kmeans-cluster [shapes]
  (loop [epoch 0 shapes shapes]
    (let [clusters (group-by :cluster shapes)
          centroids (cs/map-kv cluster->centroid clusters)
          shapes' (for [shape shapes
                        :let [gvs (grouping-vector shape)]]
                    (assoc shape
                           :cluster
                           (apply max-key
                                  (fn [cluster] (cos-similarity gvs (get centroids cluster)))
                                  (keys clusters))))]
      ;; (println [:assign epoch (cs/map-kv count clusters) colors])
      ;; (println [:assign epoch (cs/map-kv count clusters)])

      ;; iterate until fixed-point
      (if (= shapes shapes')
        shapes
        (recur (inc epoch) shapes')))))

(defn cluster->centroid-position
  [cluster]
  (tm/div (reduce tm/+ (map :position cluster))
          (inc (count cluster))))

;; FIXME: centroids trend towards 0,0 once in steady-state?
(defn update-positions [shapes]
  (let [clusters (group-by :cluster shapes)
        centroid-positions (cs/map-kv cluster->centroid-position clusters)
        sum-dist-to-centroid
        (cs/map-kv (fn [cluster]
                     (let [centroid (get centroid-positions cluster)]
                       (->> cluster
                            (map (fn [s] (g/dist (:position s) centroid)))
                            (reduce +))))
                   clusters)]
    ;;(println [:update (keys clusters) centroid-positions])
    (for [{:keys [position cluster] :as shape} shapes
          :let [centroid (get centroid-positions cluster)
                sum-dist (get sum-dist-to-centroid cluster)]]
      (assoc shape :position
             (tm/mix position centroid
                     (max 0.001 (/ (g/dist position centroid)
                                   sum-dist)))))))

(defn update-state [state]
  (update state :shapes (comp update-positions kmeans-cluster)))

(defn draw [{:keys [shapes]}]
  (q/no-stroke)
  (doseq [shape shapes]
    (draw-shape (assoc shape :theta (* 2 Math/PI (rand))))))

(sketch/defquil k-means
  :created-at "2021-03-09"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

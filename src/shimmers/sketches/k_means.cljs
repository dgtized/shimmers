(ns shimmers.sketches.k-means
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as geom]
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
      (geom/translate (cq/rel-pos position))
      (geometry/rotate-around-centroid theta)
      geom/vertices
      cq/draw-shape))

(defn make-shape []
  {:position (gv/vec2 (rand) (rand))
   :color [(rand-nth (range 0 1 0.25))
           (q/random 0.5 0.8) (q/random 0.5 0.8) 0.05]})

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

;; Generate initial triangles by sampling from video or an image?
(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsl 1.0)
  {:shapes (seed-cluster (repeatedly 256 make-shape) 6)})

(defn assign-cluster [shapes]
  (let [clusters (group-by :cluster shapes)
        colors (cs/map-kv (fn [cluster] (mapv (fn [x] (/ x (inc (count cluster))))
                                             (reduce color-add (map grouping-vector cluster))))
                          clusters)]
    ;; (println [:assign (cs/map-kv count clusters) colors])
    ;; (println [:assign (cs/map-kv count clusters)])
    (for [shape shapes
          :let [s (grouping-vector shape)]]
      (assoc shape
             :cluster
             (apply max-key
                    (fn [cluster] (cos-similarity s (get colors cluster)))
                    (keys clusters))))))

(defn update-cluster [shapes]
  (let [clusters (group-by :cluster shapes)
        positions (cs/map-kv (fn [cluster] (tm/div (reduce tm/+ (map :position cluster))
                                                  (inc (count cluster))))
                             clusters)]
    ;; (println [:update (keys clusters) positions])
    (for [{:keys [position cluster] :as shape} shapes
          :let [centroid (tm/+ (get positions cluster) (gv/randvec2 0.0003))]]
      (assoc shape :position (tm/mix position centroid 0.003)))))

(defn update-state [state]
  (update state :shapes (comp update-cluster assign-cluster)))

(defn draw [{:keys [shapes]}]
  (q/no-stroke)
  (doseq [shape shapes]
    (draw-shape (assoc shape :theta (* 2 Math/PI (rand))))))

(defn ^:export run-sketch []
  ;; 20210309
  (q/defsketch k-means
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

(ns shimmers.sketches.k-means
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn rel-h [p]
  (* (q/height) p))

(defn rel-w [p]
  (* (q/width) p))

(defn magnitude [v]
  (Math/sqrt (reduce + (map * v v))))

;; https://en.wikipedia.org/wiki/Cosine_similarity
(defn cos-similarity [v1 v2]
  (let [mag (* (magnitude v1) (magnitude v2))]
    (if (> mag 0)
      (/ (reduce + (map * v1 v2))
         mag)
      0)))

(defn draw-shape [{:keys [position shape color]}]
  (apply q/fill color)
  (cq/draw-shape (geom/vertices (geom/translate shape position))))

(defn make-shape []
  {:position (gv/vec2 (rel-w (rand)) (rel-h (rand)))
   :shape (gt/triangle2 [0 0] [0 (q/random 13 21)] [(q/random 13 21) 0])
   :color [(mod (q/random 1080) 360) (q/random 0.5 0.8) (q/random 0.5 0.8) 0.1]
   :cluster (rand-nth (range 0 8))})

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsl 360.0 1.0 1.0 1.0)
  {:shapes (repeatedly 1024 make-shape)})

(defn grouping-vector [{:keys [color position]}]
  (concat (map * color (repeat 12000)) position))

(defn color-add [c1 c2]
  (map + c1 c2))

(defn pr-dbg [v]
  (pr v)
  v)

(defn assign-cluster [shapes]
  (let [clusters (group-by :cluster shapes)
        colors (cs/map-kv (fn [cluster] (mapv (fn [x] (/ x (inc (count cluster))))
                                             (reduce color-add (map grouping-vector cluster))))
                          clusters)]
    ;; (println [:assign (cs/map-kv count clusters) colors])
    (println [:assign (cs/map-kv count clusters)])
    (map (fn [shape]
           (assoc shape
                  :cluster
                  (apply max-key
                         (fn [cluster] (cos-similarity (grouping-vector shape) (get colors cluster)))
                         (keys clusters))))
         shapes)))

(defn update-cluster [shapes]
  (let [clusters (group-by :cluster shapes)
        positions (cs/map-kv (fn [cluster] (tm/div (reduce tm/+ (map :position cluster))
                                                  (inc (count cluster))))
                             clusters)]
    ;; (println [:update (keys clusters) positions])
    (map (fn [{:keys [position cluster] :as shape}]
           (assoc shape :position (tm/mix position (get positions cluster) 0.01)))
         shapes)))

(defn update-state [state]
  (update state :shapes (comp update-cluster assign-cluster)))

(defn draw [{:keys [shapes]}]
  (q/no-stroke)
  (doseq [shape shapes]
    (draw-shape shape)))

(defn ^:export run-sketch []
  (q/defsketch k-means
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

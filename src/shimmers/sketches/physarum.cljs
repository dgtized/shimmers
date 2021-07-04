(ns shimmers.sketches.physarum
  "Implementation of behavior described in https://sagejenson.com/physarum."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.math.vector :as v]))

(defprotocol IPhysarumParticle
  (sense [_ trail])
  (rotate [_ sensors])
  (move! [_ trail bounds]))

(defrecord PhysarumParticle
    [^:mutable pos
     ^:mutable heading
     sensor-angle sensor-distance rotation step-size deposit]
  IPhysarumParticle
  (sense [_ trail]
    (for [sensor-offset [(- sensor-angle) 0 sensor-angle]]
      (->> (+ heading sensor-offset)
           (v/polar sensor-distance)
           (tm/+ pos)
           (map int)
           (get trail))))
  (rotate [_ sensors]
    (let [[left center right] sensors]
      (cond (and (> center left) (> center right)) 0
            (> left right) (- rotation)
            (< left right) rotation
            :else (rand-nth [(- rotation) 0 rotation]))))
  (move! [_ trail bounded]
    (let [sensors (sense _ trail)
          delta-heading (rotate _ sensors)
          heading' (+ heading delta-heading)
          pos' (tm/+ pos (map int (v/polar step-size heading')))]
      (set! heading heading')
      (set! pos (bounded pos'))
      _)))

(defn make-particle [pos heading]
  (map->PhysarumParticle
   {:pos pos
    :heading heading
    :sensor-angle (/ Math/PI 4)
    :sensor-distance 3.0
    :rotation (/ Math/PI 8)
    :step-size 1.5
    :deposit 1.0}))

(defn make-trail [width height]
  (reduce (fn [trail pos] (assoc trail pos 0))
          {}
          (for [i (range width)
                j (range height)]
            [i j])))

(comment (make-trail 3 3))

(defn deposit [trail particles]
  (reduce (fn [trail particle]
            (let [[x y] (:pos particle)]
              (update trail [x y] (fn [v] (min (+ v (:deposit particle)) 1.0)))))
          trail
          particles))

(comment (deposit (make-trail 3 3) [(make-particle (gv/vec2 1 1) 0)]))

(def neighbors (for [i [-1 0 1]
                     j [-1 0 1]]
                 (gv/vec2 i j)))

(defn diffuse [trail]
  (reduce-kv (fn [trail' pos value]
               (let [total (reduce (fn [sum neighbor]
                                     (+ sum (get trail (tm/+ (gv/vec2 pos) neighbor) 0.0)))
                                   0.0
                                   neighbors)]
                 (assoc trail' pos (/ (+ value total) 9.0))))
             {} trail))

(comment (diffuse (assoc (make-trail 3 3) [0 0] 1.0)))

(defn decay [trail factor]
  (reduce-kv (fn [trail' pos value] (assoc trail' pos (* value factor)))
             {}
             trail))

(comment (decay (diffuse (assoc (make-trail 3 3) [0 0] 1.0)) 0.9))

(defn wrap-edges [width height]
  (fn [[x y]]
    (gv/vec2 (cond (< x 0) width
                   (>= x width) 0
                   :else x)
             (cond (< y 0) height
                   (>= y height) 0
                   :else y))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [width 64 height 64]
    {:width width
     :height height
     :bounds (wrap-edges width height)
     :particles (repeatedly 1024
                            #(make-particle (gv/vec2 (rand-int width) (rand-int height))
                                            (rand-nth (range 0 tm/TWO_PI (/ Math/PI 2)))))
     :trail (make-trail width height)}))

(defn update-state [{:keys [particles trail bounds] :as state}]
  (doseq [p particles]
    (move! p trail bounds))
  (assoc state :trail
         (as-> trail trail
           (deposit trail particles)
           (diffuse trail)
           (decay trail 0.8))))

(defn draw [{:keys [particles trail width height]}]
  (q/no-stroke)
  (let [dx (/ (q/width) width)
        dy (/ (q/height) height)]
    (doseq [x (range width)
            y (range height)]
      (q/fill (get trail [x y]))
      (q/rect (* dx x) (* dy y) dx dy))
    #_(doseq [{:keys [pos]} particles
              :let [[x y] pos]]
        (q/fill 0 1.0 0.5)
        (q/rect (* dx x) (* dy y) dx dy))))

(sketch/defquil physarum
  :created-at "2021-07-04"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

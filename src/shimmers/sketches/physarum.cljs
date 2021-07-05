(ns shimmers.sketches.physarum
  "Implementation of behavior described in https://sagejenson.com/physarum."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.ndarray.core :as nd]))


(defn wrap-edges [width height]
  (fn [[x y]]
    [(int (tm/roundto (tm/wrap-range x width) 1.0))
     (int (tm/roundto (tm/wrap-range y height) 1.0))]))

(defprotocol IPhysarumParticle
  (sense [_ trail bounded])
  (rotate [_ sensors])
  (move! [_ trail bounds]))

(defrecord PhysarumParticle
    [^:mutable pos
     ^:mutable heading
     sensor-angle sensor-distance rotation step-size deposit]
  IPhysarumParticle
  (sense [_ trail bounded]
    (for [sensor-offset [(- sensor-angle) 0 sensor-angle]]
      (let [[x y] (bounded (->> (+ heading sensor-offset)
                                (v/polar sensor-distance)
                                (tm/+ pos)))]
        (nd/get-at trail x y))))
  (rotate [_ sensors]
    (let [[left center right] sensors]
      (cond (and (> center left) (> center right)) 0
            (> left right) (- rotation)
            (< left right) rotation
            :else (* 2 (rand-nth [(- rotation) rotation])))))
  (move! [_ trail bounded]
    (let [sensors (sense _ trail bounded)
          delta-heading (rotate _ sensors)
          heading' (+ heading delta-heading)
          pos' (tm/+ pos (v/polar step-size heading'))]
      (set! heading heading')
      (set! pos (gv/vec2 (bounded pos')))
      _)))

;; Parameters tuned from: Jones, J. (2010) Characteristics of pattern formation
;; and evolution in approximations of physarum transport networks.
;; ref:https://uwe-repository.worktribe.com/output/980579.
(defn make-particle [pos heading]
  (map->PhysarumParticle
   {:pos pos
    :heading heading
    :sensor-angle (/ Math/PI 8) ;; 22.5 degrees
    :sensor-distance 9.0
    :rotation (/ Math/PI 4) ;; 45 degrees
    :step-size 1.0
    :deposit 96}))

(defn make-trail [width height]
  (nd/ndarray :uint8-clamped
              (repeatedly (* width height) (constantly 0))
              [width height]))

(comment (nd/shape (make-trail 3 3)))

(defn deposit [trail particles]
  (doseq [{:keys [pos deposit]} particles
          :let [[x y] pos]]
    (nd/update-at trail x y
                  (fn [v] (+ v deposit))))
  trail)

(defn trail->map [trail]
  (let [[width height] (nd/shape trail)]
    (reduce (fn [m [i j]] (assoc m [i j] (nd/get-at trail i j)))
            {}
            (for [i (range width)
                  j (range height)]
              [i j]))))

(comment (trail->map (deposit (make-trail 3 3) [(make-particle (gv/vec2 1 1) 0)])))


(def neighbors (for [i [-1 0 1]
                     j [-1 0 1]
                     :when (not= i j 0)]
                 [i j]))

(defn diffuse [trail decay]
  (let [[width height] (nd/shape trail)]
    (dotimes [x width]
      (dotimes [y height]
        (let [total (reduce (fn [sum [i j]]
                              (+ (nd/get-at trail
                                            (tm/wrap-range (+ x i) width)
                                            (tm/wrap-range (+ y j) height))
                                 sum))
                            0
                            neighbors)]
          (nd/update-at trail x y
                        (fn [value] (int (* decay (/ (+ value total) 9))))))))
    trail))

(comment (trail->map (diffuse (nd/set-at (make-trail 3 3) 0 0 256) 0.8)))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (q/color-mode :hsl 1.0)
  (let [width 200 height 200]
    {:trail (make-trail width height)
     :particles (repeatedly 2048
                            #(make-particle (gv/vec2 (rand-int width) (rand-int height))
                                            (rand-nth (range 0 tm/TWO_PI 0.1))))
     :bounds (wrap-edges width height)}))

(defn update-state [{:keys [particles trail bounds] :as state}]
  (doseq [p particles]
    (move! p trail bounds))
  (deposit trail particles)
  (diffuse trail 0.8)
  state)

(defn draw [{:keys [particles trail]}]
  (q/no-stroke)
  (let [[width height] (nd/shape trail)
        dx (/ (q/width) width)
        dy (/ (q/height) height)]
    (doseq [x (range width)
            y (range height)]
      (q/fill (/ (nd/get-at trail x y) 200))
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

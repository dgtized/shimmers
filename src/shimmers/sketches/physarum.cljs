(ns shimmers.sketches.physarum
  "Implementation of behavior described in https://sagejenson.com/physarum."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn wrap-edges [[x y] width height]
  (gv/vec2 (int (tm/roundto (tm/wrap-range x width) 1.0))
           (int (tm/roundto (tm/wrap-range y height) 1.0))))

(defprotocol IPhysarumParticle
  (sense [_ trail width height])
  (rotate [_ sensors])
  (move! [_ trail width height]))

(defrecord PhysarumParticle
    [^:mutable pos
     ^:mutable heading
     sensor-angle sensor-distance rotation step-size deposit]
  IPhysarumParticle
  (sense [_ trail width height]
    (for [sensor-offset [(- sensor-angle) 0 sensor-angle]]
      (let [[x y] (->> (+ heading sensor-offset)
                       (v/polar sensor-distance)
                       (tm/+ pos))]
        (first (q/get-pixel trail
                            (tm/wrap-range x width)
                            (tm/wrap-range y height))))))
  (rotate [_ sensors]
    (let [[left center right] sensors]
      ;; Paper uses center < left & right as random case?
      (cond (and (> center left) (> center right)) 0
            (> left right) (- rotation)
            (< left right) rotation
            :else (rand-nth [(- rotation) rotation]))))
  (move! [_ trail width height]
    (let [sensors (sense _ trail width height)
          delta-heading (rotate _ sensors)
          heading' (+ heading delta-heading)
          pos' (tm/+ pos (v/polar step-size heading'))]
      (set! heading heading')
      (set! pos (wrap-edges pos' width height))
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
    :deposit 0.5}))

(defn make-trail [width height]
  (let [img (q/create-graphics width height :p2d)]
    (q/with-graphics img
      (q/color-mode :rgb 1.0)
      (q/background 0.0 1.0))
    img))

(defn deposit [trail particles]
  (doseq [{:keys [pos deposit]} particles
          :let [[x y] pos
                v (first (q/get-pixel trail x y))
                v' (tm/clamp (+ v deposit) 0.0 1.0)]]
    (q/set-pixel trail x y (q/color v' v' v' 1.0)))
  trail)

(defn diffuse [trail amount]
  (q/image-filter trail :blur amount))

(defn decay [trail width height factor]
  (dotimes [i width]
    (dotimes [j height]
      (q/set-pixel trail i j (* factor (first (q/get-pixel trail i j))))))
  (q/update-pixels trail))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (q/color-mode :rgb 1.0)
  (let [width 256
        height 256
        n-particles 1024]
    {:size [width height]
     :trail (make-trail width height)
     :buffer (q/create-graphics width height :p3d)
     :shader (q/load-shader "shaders/physarum.frag.c"
                            "shaders/physarum.vert.c")
     :width width
     :height height
     :particles
     (repeatedly n-particles
                 #(make-particle (gv/vec2 (rand-int width) (rand-int height))
                                 (rand-nth (range 0 tm/TWO_PI tm/QUARTER_PI))))}))

(defn update-state
  [{:keys [size particles trail buffer shader width height] :as state}]
  (doseq [p particles]
    (move! p trail width height))
  (deposit trail particles)
  (q/update-pixels trail)
  (let [[w h] size]
    (when (q/loaded? shader)
      (q/with-graphics buffer
        (q/shader shader)
        (q/set-uniform shader "resolution" (array w h))
        (q/set-uniform shader "trail" trail)
        (q/rect (* -0.5 w) (* -0.5 h) w h))
      (q/with-graphics trail
        (q/image buffer 0 0 w h)))
    ;; (diffuse trail 1)
    (decay trail width height 0.9)
    state))

(defn draw [{:keys [trail]}]
  (q/image trail 0 0 (q/width) (q/height)))

(sketch/defquil physarum
  :created-at "2021-07-04"
  :tags #{:shader}
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

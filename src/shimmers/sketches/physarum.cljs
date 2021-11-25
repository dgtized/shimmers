(ns shimmers.sketches.physarum
  "Implementation of behavior described in https://sagejenson.com/physarum."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.shader :as shader]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Parameters tuned from: Jones, J. (2010) Characteristics of pattern formation
;; and evolution in approximations of physarum transport networks.
;; ref:https://uwe-repository.worktribe.com/output/980579.
(defonce ui-state
  (ctrl/state {:sensor-angle 22.5
               :sensor-distance 9.0
               :rotation 45.0
               :step-size 1.0
               :deposit 0.5}))

(defn ui-controls []
  (ctrl/container
   [:h3 "Particle Parameters (after restart)"]
   (ctrl/numeric ui-state "Sensor Angle" [:sensor-angle] [0.1 360.0 0.1])
   (ctrl/numeric ui-state "Sensor Distance" [:sensor-distance] [0.1 32.0 0.1])
   (ctrl/numeric ui-state "Rotation" [:rotation] [0.1 360.0 0.1])
   (ctrl/numeric ui-state "Step Size" [:step-size] [0.1 32.0 0.1])
   (ctrl/numeric ui-state "Deposit" [:deposit] [0.1 1.0 0.1])))

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

(defn make-particle [pos]
  (map->PhysarumParticle
   (-> @ui-state
       (update-in [:sensor-angle] tm/radians)
       (update-in [:rotation] tm/radians)
       (merge {:pos pos
               :heading (rand-nth (range 0 tm/TWO_PI tm/QUARTER_PI))}))))

(defn make-trail [width height]
  (let [img (q/create-graphics width height :p2d)]
    (q/with-graphics img (q/color-mode :rgb 1.0))
    img))

(defn deposit [trail particles]
  (q/with-graphics trail
    (q/stroke-weight 1.0)
    (doseq [{:keys [pos deposit]} particles
            :let [[x y] pos
                  v (first (q/get-pixel trail x y))
                  v' (tm/clamp (+ v deposit) 0.0 1.0)]]
      (q/stroke v' v' v' 1.0)
      (q/point x y)))
  trail)

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (let [width 200
        height 200
        n-particles 1024]
    {:trail (make-trail width height)
     :buffer (q/create-graphics width height :p3d)
     :shader (q/load-shader "shaders/physarum.frag.c"
                            "shaders/physarum.vert.c")
     :width width
     :height height
     :particles
     (repeatedly n-particles #(make-particle (cq/rel-vec (rand) (rand))))}))

(defn update-state
  [{:keys [particles trail buffer shader width height] :as state}]
  (doseq [p particles]
    (move! p trail width height))
  (deposit trail particles)
  (shader/transform shader buffer trail [width height]
                    {"resolution" (array width height)
                     "trail" trail
                     "decay" 0.95})
  state)

(defn draw [{:keys [trail]}]
  (q/image trail 0 0 (q/width) (q/height)))

(sketch/defquil physarum
  :created-at "2021-07-04"
  :tags #{:shader}
  :size [800 800]
  :on-mount (fn [] (ctrl/mount ui-controls))
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

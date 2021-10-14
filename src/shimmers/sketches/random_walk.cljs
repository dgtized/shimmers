(ns shimmers.sketches.random-walk
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.particle-system :as particles]
            [shimmers.common.quil :as cq]
            [shimmers.math.color :as color]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

(defn make-particle []
  (let [initial-pos (cq/rel-vec (rand) (rand))]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (v/vec2 (q/random-2d))
     :acceleration (v/vec2 (q/random-2d))
     :color (color/random)}))

(defn constrain2d [[x y] lower upper]
  (v/vec2 (tm/clamp x lower upper)
          (tm/clamp y lower upper)))

(defn update-particle
  [{:keys [position velocity acceleration] :as particle}]
  (let [new-velocity (-> (v/add velocity acceleration) (constrain2d -1.5 1.5))
        new-position (v/add position new-velocity)
        wrapped-position (v/wrap2d new-position (q/width) (q/height))]
    (assoc particle
           :last-pos (if (= wrapped-position new-position) position wrapped-position)
           :position wrapped-position
           :velocity new-velocity
           :acceleration (v/scale (v/vec2 (q/random-2d)) 0.5))))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 50 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw [{:keys [particles]}]
  ;; (q/background 256 16)
  (particles/draw particles))

(sketch/defquil random-walk
  :created-at "2020-10-21"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])



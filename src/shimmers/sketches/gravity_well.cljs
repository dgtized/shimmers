(ns shimmers.sketches.gravity-well
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]))

(defrecord Body
    [position last-pos velocity acceleration mass color])

(defn make-body [position mass color]
  (map->Body
   {:last-pos position
    :position position
    :velocity (v/vec2 0 0)
    :acceleration (v/vec2 0 0)
    :mass mass
    :color color}))

(defn make-sun [pos]
  (make-body pos 1000 [200 200 0 96]))

(defn make-random-body []
  (let [r 150]
    (make-body (v/vec2 (q/random (- r) r)
                       (q/random (- r) r))
               (q/random 1 5)
               [0 0 0 96])))

(defn gravitational-pull
  [{:keys [position mass] :as current} bodies]
  (reduce v/add
          (v/vec2 0 0)
          (for [body bodies
                :when (not= body current)
                :let [gravity 0.006
                      d2 (tg/dist-squared position (:position body))]]
            (v/scale (tm/normalize (tm/- (:position body) position))
                     (/ (* gravity (:mass body) mass)
                        (max d2 1))))))

(defn update-body
  [bodies {:keys [position velocity acceleration mass] :as body}]
  (let [new-velocity (v/add velocity acceleration)
        new-position (v/add position new-velocity)]
    (assoc body
           :last-pos position
           :position new-position
           :velocity new-velocity
           :acceleration (v/scale (gravitational-pull body bodies)
                                  (/ 0.1 mass)))))

(defn visible? [body]
  (< (tg/dist (:position body) (v/vec2 0 0)) 400))

(defn restart-sim? [{:keys [start-frame bodies]} frame-count]
  (let [age (- frame-count start-frame)
        visible (count (filter visible? bodies))]
    (or (> age 12000) (< visible 16))))

(defn setup []
  (q/background 255)
  {:start-frame (q/frame-count)
   :bodies (into (repeatedly 128 make-random-body)
                 (rand-nth
                  [[]
                   [(make-sun (v/vec2 0 0))]
                   [(make-sun (v/vec2 -200 0))
                    (make-sun (v/vec2 200 0))]]))})

(defn update-state [state]
  (if (restart-sim? state (q/frame-count))
    (setup)
    (update state :bodies (fn [bodies] (map (partial update-body bodies) bodies)))))

(defn draw-bodies [bodies]
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [{:keys [position last-pos color mass]} bodies]
    (apply q/stroke color)
    (q/stroke-weight (q/map-range mass 1 1000 1 15))
    (q/line last-pos position)))

(defn draw [{:keys [bodies]}]
  (q/background 255 24)
  (draw-bodies bodies))

(defn ^:export run-sketch []
  (q/defsketch gravity-well
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))


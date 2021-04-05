(ns shimmers.sketches.gravity-well
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as geom]
            [thi.ng.math.core :as tm]
            [shimmers.common.particle-system :as particles]))

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
               (q/random 1 8)
               [0 0 0 96])))

(defn gravitational-pull
  [{:keys [position mass] :as current} bodies]
  (reduce v/add
          (v/vec2 0 0)
          (for [body bodies
                :when (not= body current)
                :let [gravity 0.004
                      d2 (geom/dist-squared position (:position body))]]
            (v/scale (tm/normalize (tm/- (:position body) position))
                     (/ (* gravity (:mass body) mass)
                        (max d2 2))))))

(defn update-body
  [bodies {:keys [mass] :as body}]
  (-> body
      (assoc :acceleration (v/scale (gravitational-pull body bodies)
                                    (/ 1.0 mass)))
      particles/step))

(defn visible? [body]
  (< (geom/dist (:position body) (v/vec2 0 0)) 400))

(defn restart-sim? [{:keys [start-frame bodies]} frame-count]
  (let [age (- frame-count start-frame)
        visible (count (filter visible? bodies))]
    (or (> age 12000) (< visible 16))))

(defn setup []
  (q/background 255)
  {:start-frame (q/frame-count)
   :bodies (into (repeatedly 100 make-random-body)
                 (rand-nth
                  [[]
                   [(make-sun (v/vec2 0 0))]
                   [(make-sun (v/vec2 -200 0))
                    (make-sun (v/vec2 200 0))]]))})

(defn update-state [state]
  (if (restart-sim? state (q/frame-count))
    (setup)
    (update state :bodies (fn [bodies] (map (partial update-body bodies) bodies)))))

(defn draw [{:keys [bodies]}]
  (q/background 255 24)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [max-mass (apply max (map :mass bodies))
        weight-fn (fn [{:keys [mass]}] (q/map-range mass 1 max-mass 1 6))]
    (particles/draw bodies :weight weight-fn)))

(defn ^:export run-sketch []
  (q/defsketch gravity-well
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))


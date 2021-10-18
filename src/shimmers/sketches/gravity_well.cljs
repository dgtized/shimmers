(ns shimmers.sketches.gravity-well
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.particle-system :as particles]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
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
  (make-body pos 1e9 [200 200 0 96]))

(defn make-random-body []
  (let [r (* 0.4 (q/height))]
    (make-body (v/vec2 (q/random (- r) r)
                       (q/random (- r) r))
               (q/random 1e3 1e4)
               [0 0 0 96])))

(defn gravitational-pull
  [{:keys [position mass] :as current} bodies]
  (let [forces
        (for [body bodies
              :when (not= body current)
              :let [gravity 9.8
                    d2 (g/dist-squared position (:position body))]]
          (tm/normalize (tm/- (:position body) position)
                        (/ (* gravity (:mass body) mass)
                           (max d2 2))))]
    (tm/div (reduce v/add (v/vec2 0 0) forces)
            (count bodies))))

(defn update-body
  [bodies {:keys [mass] :as body}]
  (-> body
      (assoc :acceleration (tm/div (tm/div (gravitational-pull body bodies) mass)
                                   (/ (reduce + (map :mass bodies)) (dec (count bodies)))))
      particles/step))

(defn visible? [body]
  (< (g/dist (:position body) (v/vec2 0 0)) (q/height)))

(defn restart-sim? [{:keys [start-frame bodies]} frame-count]
  (let [age (- frame-count start-frame)
        visible (count (filter visible? bodies))]
    (or (> age 12000) (< visible 16))))

(defn setup []
  (q/background 255)
  (let [w (* 0.3 (q/width))]
    {:start-frame (q/frame-count)
     :bodies (into (repeatedly 100 make-random-body)
                   (rand-nth
                    [[]
                     [(make-sun (v/vec2 0 0))]
                     [(make-sun (v/vec2 (- w) 0))
                      (make-sun (v/vec2 w 0))]]))}))

(defn update-state [state]
  (if (restart-sim? state (q/frame-count))
    (setup)
    (update state :bodies (fn [bodies] (map (partial update-body bodies) bodies)))))

(defn draw [{:keys [bodies]}]
  (q/background 255 24)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [max-mass (apply max (map :mass bodies))
        weight-fn (fn [{:keys [mass]}] (q/map-range mass 1 max-mass 1.5 9))]
    (particles/draw bodies :weight weight-fn)))

(sketch/defquil gravity-well
  :created-at "2021-01-06"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])


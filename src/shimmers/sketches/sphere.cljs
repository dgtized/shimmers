(ns shimmers.sketches.sphere
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.math.core :as tm]))

(defn setup []
  {:radius 150
   :vertice-count 48})

(defn update-state [state]
  (assoc state :percent (tm/mix-cosine 0 1 (/ (q/millis) 10000))))

(defn sphere-points [vertice-count]
  (for [i (range vertice-count)
        :let [longitude (tm/map-interval i 0 vertice-count (- Math/PI) Math/PI)]
        j (range vertice-count)
        :let [latitude (tm/map-interval j 0 vertice-count (- (/ Math/PI 2)) (/ Math/PI 2))]]
    [(* (Math/sin longitude) (Math/cos latitude))
     (* (Math/sin longitude) (Math/sin latitude))
     (Math/cos longitude)]))

(defn draw [{:keys [radius vertice-count percent]}]
  (q/orbit-control)
  (q/background 255)
  (q/stroke 0)
  (q/rotate-x 0.6)
  (q/rotate-y -0.2)
  (q/rotate-z (/ (q/frame-count) 1000))
  (let [points (sphere-points vertice-count)]
    (doseq [position (take (* percent (count points)) points)]
      (apply q/point (map (partial * radius) position)))))

(defn ^:export run-sketch []
  (q/defsketch sphere-sketch
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

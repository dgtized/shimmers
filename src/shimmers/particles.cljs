(ns shimmers.particles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.vector :as v]))

(defn wrap-around [[x y]]
  [(v/wrap-value x 0 (q/width))
   (v/wrap-value y 0 (q/height))])

(def colors [[128 32]
             [128 0 0 32]
             [0 0 128 32]
             [0 128 0 32]])

(defn make-particle []
  (let [initial-pos [(q/random (q/width)) (q/random (q/height))]]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (q/random-2d)
     :acceleration [0 0]
     :mass (q/random 1 20)
     :color [32 128]}))

(defn force-at-position [[x y]]
  (let [n (q/noise (/ x 100) (/ y 100)
                   (/ (q/frame-count) 500))
        r (* 8 Math/PI n)]
    [(q/cos r) (q/sin r)]))

(defn update-particle
  [{:keys [position velocity acceleration mass] :as particle}]
  (let [new-velocity (-> (v/add velocity acceleration) (v/constrain2d -1.5 1.5))
        new-position (v/add position new-velocity)
        wrapped-position (wrap-around new-position)
        force (force-at-position wrapped-position)]
    (assoc particle
           :last-pos (if (= wrapped-position new-position) position wrapped-position)
           :position wrapped-position
           :velocity new-velocity
           :acceleration (v/scale force (* 0.3 (/ mass 100))))))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 250 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw-forces []
  (q/background 256 4)
  (q/stroke 200 50)
  (doseq [x (range 0 400 10)
          y (range 0 300 10)
          :let [[fx fy] (force-at-position [x y])]]
    (q/line x y (+ x (* 5 fx)) (+ y (* 5 fy)))))

(defn draw-particles [particles]
  (doseq [{:keys [position last-pos color mass]} particles]
    (apply q/stroke color)
    (q/stroke-weight (/ mass 100.0))
    (let [[lx ly] last-pos
          [x y] position]
      (q/line lx ly x y))))

(defn draw [{:keys [particles]}]
  #_(draw-forces)
  (draw-particles particles)
  )

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [400 300]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))



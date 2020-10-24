(ns shimmers.particles
  "Loosely derived from Coding Challenge #24: Perlin Noise Flow Field
  https://www.youtube.com/watch?v=BjoM9oKOAKY"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.vector :as v]))

(defn wrap-around [[x y]]
  [(v/wrap-value x 0 (q/width))
   (v/wrap-value y 0 (q/height))])

(def colors [[128 192 128 64]
             [128 0 0 64]
             [0 128 0 64]
             [0 0 128 64]
             [0 0 192 64]])

(defn make-particle []
  (let [initial-pos [(q/random (q/width)) (q/random (q/height))]]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (q/random-2d)
     :acceleration [0 0]
     :mass (q/random 1 50)
     :color (rand-nth colors)}))

(defn stokes-drag [velocity]
  "Viscous resistance is a negative force proportional to velocity.
From https://en.wikipedia.org/wiki/Drag_(physics)
"
  ;; (v/add velocity (v/scale velocity -0.1))
  (v/scale velocity 0.90)
  )

(defn force-at-position [[x y]]
  (let [n (q/noise (/ x 100) (/ y 100)
                   (/ (q/frame-count) 500))
        r (* 8 Math/PI n)]
    [(q/cos r) (q/sin r)]))

(defn acceleration-at-point [{:keys [position mass]}]
  (let [force (force-at-position position)
        brownian (v/scale (q/random-2d) 0.005)]
    (v/add (v/scale force (* 0.8 (/ mass 100)))
           brownian)))

(defn update-particle
  [{:keys [position velocity acceleration mass] :as particle}]
  (let [new-velocity (stokes-drag (v/add velocity acceleration))
        new-position (v/add position new-velocity)
        wrapped-position (wrap-around new-position)]
    (assoc particle
           :last-pos (if (= wrapped-position new-position) position wrapped-position)
           :position wrapped-position
           :velocity new-velocity
           :acceleration (acceleration-at-point particle))))

(defn make-user-interface [{:keys [ui] :as state}]
  (let [forces (-> (quil.sketch/current-applet)
                   (.createCheckbox "Draw Forces" (:draw-forces @ui)))
        background-div (-> (quil.sketch/current-applet) (.createDiv "Background Opacity"))
        background (-> (quil.sketch/current-applet)
                       (.createSlider 0 256 (:opacity @ui) 1))]
    ;; Use https://p5js.org/reference/#/p5/changed to toggle :draw-forces
    (.changed forces (fn [] (swap! ui assoc :draw-forces (.checked forces))))
    (.changed background (fn [] (swap! ui assoc :opacity (.value background))))
    state))

(defn setup []
  (q/background "white")
  (make-user-interface
   {:particles (repeatedly 1000 make-particle)
    :ui (atom {:draw-forces false
               :opacity 0})}))

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw-forces []
  (q/stroke-weight 0.33)
  (q/stroke 0 5)
  (doseq [x (range 0 400 10)
          y (range 0 300 10)
          :let [[fx fy] (force-at-position [x y])]]
    (q/line x y (+ x (* 5 fx)) (+ y (* 5 fy)))))

(defn draw-particles [particles]
  (doseq [{:keys [position last-pos color mass]} particles]
    (apply q/stroke color)
    (q/stroke-weight (/ mass 300.0))
    (let [[lx ly] last-pos
          [x y] position]
      (q/line lx ly x y))))

(defn draw [{:keys [particles ui]}]
  (let [opacity (:opacity @ui)]
    (q/background 256 opacity))
  (when (:draw-forces @ui)
    (draw-forces))
  (draw-particles particles))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [400 300]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))

(comment
  ;; kill existing sketch
  (q/with-sketch (q/get-sketch-by-id "quil-host")
    (q/exit)))



(ns shimmers.sketches.particles
  "Loosely derived from Coding Challenge #24: Perlin Noise Flow Field
  https://www.youtube.com/watch?v=BjoM9oKOAKY"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.color :as color]
            [shimmers.math.core :as sm]
            [shimmers.math.vector :as v]
            [shimmers.common.particle-system :as particles]))

;; random distribution between 1 and 20 units of mass
(def mass-range [1.0 20.0])

(defn make-particle []
  (let [initial-pos (v/vec2 (q/random (q/width)) (q/random (q/height)))]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (v/vec2 0 0)
     :acceleration (v/vec2 0 0)
     :mass (apply q/random mass-range)
     ;; :color (color/random)
     :color (color/random-gradient :blue-cyan)}))

(defn stokes-drag [velocity]
  "Viscous resistance is a negative force proportional to velocity.
From https://en.wikipedia.org/wiki/Drag_(physics)
"
  ;; (v/add velocity (v/scale velocity -0.1))
  (v/scale velocity 0.90))

;; Because of discontinuity when noise wraps around, there were often competing
;; forces at the borders. We fix this by reflecting x and y coordinates around
;; the halfway point for the purposes of calculating noise to ensure they are
;; continuous at edges.
(defn force-at-position [[x y]]
  (let [factor 100
        rx (sm/reflect-into x (q/width))
        ry (sm/reflect-into y (q/height))
        n (q/noise (/ rx factor) (/ ry factor)
                   (/ (q/frame-count) 2000))
        r (* 4 Math/PI n)]
    (v/vec2 (q/cos r) (q/sin r))))

(defn acceleration-at-point [{:keys [position mass]}]
  (let [;; Pretending that wind-force at a position is inversely proportional to
        ;; mass of object ie a = F/m. It's not particularly correct as a rule of
        ;; physics, but it looks nice if the larger objects have slower
        ;; acceleration.
        wind (v/scale (force-at-position position) (/ 1 mass))
        ;; Arbitrarily making additional random hops in some small direction
        ;; inversely proportional to mass.
        brownian (v/scale (v/vec2 (q/random-2d)) (/ 0.1 mass))]
    (v/add wind brownian)))

(defn update-particle
  [{:keys [position velocity acceleration mass] :as particle}]
  (let [new-velocity (stokes-drag (v/add velocity acceleration))
        new-position (v/add position new-velocity)
        wrapped-position (v/wrap2d new-position (q/width) (q/height))]
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
    :particle-graphics (q/create-graphics (q/width) (q/height))
    :ui (atom {:draw-forces false
               :opacity 0})}))

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw-forces []
  (q/stroke-weight 0.33)
  (q/stroke 0 128)
  (let [cols (q/floor (/ (q/width) 20))
        hcols (/ cols 2)
        len (/ cols 4)]
    (doseq [x (range 0 (q/width) cols)
            y (range 0 (q/height) cols)
            :let [force (force-at-position [x y])
                  from (v/add (v/vec2 x y) (v/vec2 hcols hcols))]]
      (q/line from (v/add from (v/scale force len))))))

(defn draw [{:keys [particles ui particle-graphics]}]
  (let [opacity (:opacity @ui)]
    (q/with-graphics particle-graphics
      (q/background 256 opacity)
      (let [[lightest heaviest] mass-range
            weight-fn (fn [{:keys [mass]}]
                        (q/map-range mass lightest heaviest 0.2 0.5))]
        (particles/draw particles :weight weight-fn))))

  (q/background 256)
  (q/image particle-graphics 0 0)
  (when (:draw-forces @ui)
    (draw-forces)))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))


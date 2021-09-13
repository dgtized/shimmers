(ns shimmers.sketches.motion-of-insects
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]
            [shimmers.math.verlet-particles :as vp]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn wrap-around [width height]
  (fn [p _]
    (let [pos (:pos p)
          [x y] pos
          wrapped (gv/vec2 (tm/wrap-range x width)
                           (tm/wrap-range y height))]
      (when-not (tm/delta= pos wrapped)
        (set! (.-prev p) (tm/- wrapped (vp/velocity p)))
        (set! (.-pos p) wrapped)))))

(defn make-insect []
  (let [p (cq/rel-vec (tm/random) (tm/random))]
    (vp/make-particle p (tm/+ p (v/jitter 0.1)) 1.0)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:system
   (vp/make-system {:particles (repeatedly 100 make-insect)
                    :mechanics [(wrap-around (q/width) (q/height))]
                    :constraints []
                    :drag 0.05})})

(defn update-state [{:keys [system] :as state}]
  (vp/timestep system 2)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [pos]} (:particles system)]
    (cq/circle pos 8.0)))

(sketch/defquil motion-of-insects
  :created-at "2021-08-13"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

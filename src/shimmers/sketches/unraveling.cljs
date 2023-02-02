(ns shimmers.sketches.unraveling
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

;; Reminiscent of Yoann Bourgeois "Progress is not Linear" dance

(defn spiral-inside [t0 dr dt]
  (let [radius (cq/rel-h 0.48)
        center (cq/rel-vec 0.5 0.5)
        circle (gc/circle center radius)]
    (->> {:circle circle :t t0 :r radius}
         (iterate
          (fn [{:keys [circle t r]}]
            (let [r' (* dr r)]
              {:circle (gc/circle (v/+polar (g/point-at circle (/ t eq/TAU)) r' (+ t Math/PI)) r')
               :t (+ t dt)
               :r r'})))
         (take-while (fn [{:keys [r]}] (> r 3.0)))
         (map :circle))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [{:keys [t] :as state}]
  (update state :t + (+ 0.002 (* 0.018 (eq/unit-cos (+ tm/PHI (/ t tm/PHI)))))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight (/ 1 tm/PHI))
  (doseq [circle (spiral-inside (* tm/PHI t)
                                (+ 0.79 (* 0.175 (eq/unit-cos t)))
                                (+ 0.01 (* 0.5 (eq/unit-cos (* tm/PHI t)))))]
    (cq/circle circle)))

(sketch/defquil unraveling
  :created-at "2023-02-02"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

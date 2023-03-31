(ns shimmers.sketches.motion-control
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.control :as control]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defrecord Particle [pos angle vel angle-vel dest])

(defn move [pos-c angle-c drag]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle} dt]
    (let [force (control/force-accel pos dest pos-c vel)
          angle-target (g/heading (tm/- dest pos))
          angle-acc (control/angular-acceleration angle angle-target angle-c angle-vel)
          drag-c (- 1.0 (eq/sqr (* drag dt)))]
      (-> particle
          (assoc
           :pos (tm/+ pos (tm/* vel dt))
           :angle (+ angle (* angle-vel dt))
           :vel (tm/* (tm/+ vel (tm/* force dt)) drag-c)
           :angle-vel (* (+ angle-vel (* angle-acc dt)) drag-c))))))

(defn gen-location []
  (cq/rel-vec (dr/rand-nth [0.2 0.8]) 0.5))

(defn update-particle [{:keys [pos dest] :as particle} motion-fn dt]
  (-> (if (and (< (g/dist pos dest) 1.0) (dr/chance 0.1))
        (assoc particle :dest (gen-location))
        particle)
      (motion-fn dt)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:motion-fn (move 0.5 0.5 0.9)
   :particle (->Particle (gen-location) (dr/random-tau) (gv/vec2) 0.0 (gen-location))})

(defn update-state [{:keys [motion-fn] :as state}]
  (let [dt 0.1]
    (update state :particle update-particle motion-fn dt)))

(defn draw-particle [{:keys [pos angle dest]}]
  (q/color 0.0)
  (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r 10} angle))
  (q/color 0.0 0.5 0.5)
  (cq/circle dest 3.0))

(defn draw [{:keys [particle]}]
  (q/background 1.0)
  (draw-particle particle))

(defn page []
  [:div
   [:div.contained.explanation
    [:p "Seeking behavior using control/force-accel"]]
   (sketch/component
    :size [600 100]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition motion-control
  {:created-at "2023-03-31"
   :type :quil
   :tags #{}}
  (ctrl/mount page "sketch-host"))

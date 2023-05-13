(ns shimmers.sketches.motion-control
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.control :as control]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [fipp.ednize :refer [IEdn]]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state {}))

(defrecord Particle [pos angle vel angle-vel dest])

(extend-protocol IEdn
  Particle
  (-edn [s]
    (tagged-literal 'Particle (debug/untyped s)))
  control/PID
  (-edn [s]
    (tagged-literal 'PID (debug/untyped s))))

(defn move [pos-c angle-c drag]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle} _ dt]
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

(defn pid-mover
  [{:keys [pos angle vel angle-vel dest] :as particle} t dt]
  (let [drag (- 1.0 (eq/sqr 0.02))
        pos-pid
        (control/adjust (get particle :pos-pid
                             (control/make-pid {:kp 2.0 :ki 0.05 :kd 1.0
                                                :set-point (:x dest)
                                                :bounds [-2000.0 2000.0 -20.0 20.0]}))
                        (* t 1000.0)
                        (:x pos))
        force (gv/vec2 (get pos-pid :control 0.0) 0)
        angle-target (g/heading (tm/- dest pos))
        angle-pid
        (control/adjust (get particle :angle-pid
                             (control/make-pid {:kp 1.0 :ki 0.5 :kd 0.5
                                                :set-point angle-target
                                                :bounds [(- eq/TAU) eq/TAU -1.0 1.0]}))
                        (* t 1000.0)
                        (mod angle eq/TAU))
        angle-acc (get angle-pid :control 0.0)]
    (reset! defo particle)
    (assoc particle
           :pos-pid (assoc pos-pid :set-point (:x dest))
           :angle-pid (assoc angle-pid :set-point angle-target)
           :pos (tm/+ pos (tm/* vel dt))
           :angle (+ angle (* angle-vel dt))
           :vel (tm/* (tm/+ vel (tm/* force dt)) drag)
           :angle-vel (* (+ angle-vel (* angle-acc dt)) drag))))

;; TODO: add thrust example where rotation is small dv changes and only a rear
;; thruster can push forward

;; TODO: add a mix or linear tweening between src and dest
;; TODO: add example mixing from particle to dest
;; TODO: add force push avoidance?
;; TODO: add something with overshoot / correction?
;; TODO: include a speed limit?

;; See also http://motion.cs.illinois.edu/RoboticSystems/PlanningWithDynamicsAndUncertainty.html
;; https://motion.cs.illinois.edu/RoboticSystems/Geometry.html
;; https://motion.cs.illinois.edu/RoboticSystems/Control.html
;; Also investigate Fast Marching Tree algorithm aka FMT*
;; * https://blog.nodraak.fr/2020/12/aerospace-sim-2-guidance-law/
;; https://en.wikipedia.org/wiki/PID_controller -- Cascade control?

(defn gen-location []
  (cq/rel-vec (dr/rand-nth [0.2 0.8]) 0.5))

(defn update-particle [{:keys [pos dest] :as particle} motion-fn t dt]
  (-> (if (and (< (g/dist pos dest) 4.0) (dr/chance 0.1))
        (assoc particle :dest (gen-location))
        particle)
      (motion-fn t dt)))

(defn setup [motion-fn]
  (fn []
    (q/color-mode :hsl 1.0)
    {:motion-fn motion-fn
     :t 0.0
     :particle (->Particle (gen-location) (dr/random-tau) (gv/vec2) 0.0 (gen-location))}))

(defn update-state [{:keys [motion-fn t] :as state}]
  (let [dt 0.01]
    (-> state
        (update :particle update-particle motion-fn t dt)
        (update :t + dt))))

(defn draw-particle [{:keys [pos angle dest]}]
  (q/color 0.0)
  (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r 10} angle))
  (q/color 0.0 0.5 0.5)
  (cq/circle dest 3.0))

(defn draw [{:keys [particle]}]
  (q/background 1.0)
  (draw-particle particle))

(defn move-example [vel-c angle-c drag]
  [:<>
   [:div.contained.explanation
    [:p "Seeking behavior using move " vel-c " " angle-c " " drag]]
   (sketch/component
    :size [600 100]
    ;; :performance-id :fps-overlay
    :setup (setup (move vel-c angle-c drag))
    :update update-state
    :draw draw
    :middleware [m/fun-mode])])

(defn pid-example []
  [:<>
   [:div.contained.explanation
    [:p "PID Control"]]
   (sketch/component
    :size [600 100]
    :setup (setup pid-mover)
    :update update-state
    :draw draw
    :middleware [m/fun-mode])
   [:div.contained.explanation
    (let [particle @defo]
      [:div.flexcols
       [:div {:style {:width "15em"}}
        "Telemetry"
        [debug/pre-edn (dissoc particle :pos-pid :angle-pid)]]
       [:div {:style {:width "15em"}}
        "Position"
        [debug/pre-edn (get particle :pos-pid)]]
       [:div {}
        "Angle"
        [debug/pre-edn (get particle :angle-pid)]]])
    ]])

;; TODO: add examples using PID control for acceleration to position
(defn page []
  [:div
   [pid-example]
   (move-example 0.1 0.1 0.9)
   (move-example 0.5 0.5 0.9)
   (move-example 1.2 1.2 0.9)])

(sketch/definition motion-control
  {:created-at "2023-03-31"
   :type :quil
   :tags #{}}
  (ctrl/mount page))

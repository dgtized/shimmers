(ns shimmers.sketches.balloon
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:pressure 15.0
   :mass 0.25
   :balloon (mapv (fn [pos] {:pos pos :vel (gv/vec2)})
                  (g/vertices (gc/circle (cq/rel-vec 0.5 0.33) (cq/rel-h 0.15)) 24))})

;; fake dampening factor for velocity to prevent chaos
(def dampen 0.90)
(def gravity (gv/vec2 0.0 9.8))

;; TODO: add a kinematic chain to tether to the ground and adjust density from heat?
;; also add a bounding box to ensure it squishes against rigid body?

(defn pressurize [pressure mass dt]
  (fn [balloon]
    (let [volume (g/area (gp/polygon2 (mapv :pos balloon)))
          density (/ mass volume)
          points (mapv (fn [p]
                         (assoc p :force (tm/- (tm/* gravity mass)
                                               (tm/* gravity (* density volume)))))
                       balloon)]
      (->> points
           cs/triplet-cycle
           (map (fn [[prev current next]]
                  (let [pprev (:pos prev)
                        pnext (:pos next)
                        pcurr (:pos current)
                        outward (tm/normalize
                                 (g/normal (tm/- pprev pnext))
                                 pressure)
                        k -2.0
                        left (tm/* (tm/- pcurr pprev) k)
                        right (tm/* (tm/- pcurr pnext) k)
                        elastic (tm/+ left right)
                        force (tm/+ (:force current) (tm/+ outward elastic))
                        vel (:vel current)
                        vel' (tm/* (tm/+ vel (tm/* force dt)) dampen)
                        pos' (tm/+ (:pos current) (tm/* vel dt))]
                    (assoc current :pos pos' :vel vel'))))))))

(defn update-state [{:keys [t pressure mass] :as state}]
  (let [dt 0.1]
    (-> state
        (update :balloon (pressurize pressure mass dt))
        (update :t + dt)
        (assoc :pressure (+ 15.0 (* 5.0 (Math/cos (* 0.05 t))))))))

(defn draw [{:keys [balloon]}]
  (q/background 1.0)
  (q/no-fill)
  (let [points (map :pos balloon)]
    (cq/draw-curve-shape points)
    (q/fill 0.0)
    (doseq [p points]
      (cq/circle p 3.0))))

(defn ui-controls []
  [:div
   [:p.readable-width "Modeling a balloon using point forces across the surface.
   The pressure of the balloon varies over time, causing it to expand and
   contract."]])

(sketch/defquil balloon
  :created-at "2023-03-01"
  :tags #{}
  :size [800 600]
  :on-mount (fn [] (ctrl/mount ui-controls))
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

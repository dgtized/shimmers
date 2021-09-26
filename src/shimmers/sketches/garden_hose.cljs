(ns shimmers.sketches.garden-hose
  "Concept is a randomly generated hose and then slowly unwind as water flows through it."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.kinematic-chain :as chain]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn constrain [bounds [x y]]
  (gv/vec2 (tm/clamp x (rect/left bounds) (rect/right bounds))
           (tm/clamp y (rect/bottom bounds) (rect/top bounds))))

(defn next-point [bounds variance {:keys [angle length] :as segment}]
  (loop [variance variance]
    (let [theta (mod (p/gaussian angle variance) tm/TWO_PI)
          endpoint (chain/segment-endpoint (assoc segment :angle theta))]
      (if (geom/contains-point? bounds endpoint)
        (chain/->KinematicSegment endpoint theta length)
        (recur (+ variance 0.01))))))

(defn make-hose [n segment next-point]
  (->> segment
       (iterate next-point)
       (take n)
       chain/->KinematicChain))

(defn hose-pressure [hose clamped pressure]
  (let [segments (:segments hose)]
    (assoc hose :segments
           (conj
            (mapv (fn [[{base :base a-theta :angle length :length :as a}
                       {b-theta :angle target :base}]]
                    (let [diff (- b-theta a-theta)
                          change (* (/ (Math/abs diff) tm/TWO_PI) diff)
                          new-angle (+ a-theta change)
                          new-base (clamped (tm/mix base (tm/- target (chain/project new-angle length)) pressure))]
                      (assoc a
                             :base new-base
                             :angle (geom/angle-between new-base target))))
                  (partition 2 1 segments))
            (last segments)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.9 0.9))]
    {:start (cq/rel-vec 0.5 0.15)
     :target (cq/rel-vec 0.5 0.85)
     :bounds bounds
     :hose (make-hose 512 (chain/->KinematicSegment (cq/rel-vec 0.5 0.5) tm/HALF_PI 8)
                      (partial next-point bounds 0.6))}))

(defn update-state [{:keys [start target bounds hose] :as state}]
  ;; (println (:angle (first (:segments (:hose state)))))
  (let [segments (-> hose :segments)
        first-pos (constrain bounds (tm/mix (:base (first segments)) start 0.0))
        last-pos (constrain bounds (tm/mix (chain/segment-endpoint (last segments)) target 0.01))]
    (-> state
        (update :hose hose-pressure (partial constrain bounds) 0.02)
        (update :hose chain/chain-update first-pos last-pos))))

(defn draw [{:keys [hose]}]
  (q/background 1.0 0.15)
  (q/no-fill)
  (cq/draw-vertices (geom/vertices hose)))

(sketch/defquil garden-hose
  :created-at "2021-09-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

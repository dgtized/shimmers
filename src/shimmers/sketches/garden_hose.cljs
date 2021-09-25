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
            [thi.ng.math.core :as tm]))

(defn next-point [bounds variance {:keys [angle length] :as segment}]
  (loop [variance variance]
    (let [theta (p/gaussian angle variance)
          endpoint (chain/segment-endpoint (assoc segment :angle theta))]
      (if (geom/contains-point? bounds endpoint)
        (chain/->KinematicSegment endpoint theta length)
        (recur (+ variance 0.01))))))

(defn make-hose [n segment next-point]
  (->> segment
       (iterate next-point)
       (take n)
       chain/->KinematicChain))

(defn hose-pressure [hose pressure]
  (let [segments (:segments hose)]
    (assoc hose :segments
           (into (take 1 segments)
                 (map (fn [[{a-theta :angle} {b-theta :angle :as b}]]
                        (let [diff (- a-theta b-theta)
                              change (* (/ (mod (Math/abs diff) tm/PI) tm/PI) pressure diff)]
                          (assoc b :angle (+ b-theta change))))
                      (partition 2 1 segments))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [base (cq/rel-vec 0.5 0.15)
        bounds (rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.9 0.9))]
    {:base base
     :bounds bounds
     :hose (make-hose 512 (chain/->KinematicSegment base tm/HALF_PI 8)
                      (partial next-point bounds 0.6))}))

(defn update-state [{:keys [base] :as state}]
  (-> state
      (update :hose hose-pressure 0.1)
      (update :hose chain/chain-update base nil)))

(defn draw [{:keys [hose]}]
  (q/background 1.0 0.1)
  (q/no-fill)
  (cq/draw-vertices (geom/vertices hose)))

(sketch/defquil garden-hose
  :created-at "2021-09-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

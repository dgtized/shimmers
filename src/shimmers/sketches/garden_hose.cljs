(ns shimmers.sketches.garden-hose
  "Concept is a randomly generated hose and then slowly unwind as water flows through it."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.kinematic-chain :as chain]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as tm]))

(defn next-point [bounds variance {:keys [angle length] :as segment}]
  (loop [variance variance]
    (let [theta (mod (p/gaussian angle variance) tm/TWO_PI)
          endpoint (chain/segment-endpoint (assoc segment :angle theta))]
      (if (g/contains-point? bounds endpoint)
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
                          change (* (/ (Math/abs diff) tm/PI) diff)
                          new-angle (+ a-theta change)
                          new-base (clamped (tm/mix base (tm/- target (chain/project new-angle length)) pressure))]
                      (assoc a
                             :base new-base
                             :angle (g/angle-between new-base target))))
                  (partition 2 1 segments))
            (last segments)))))

(defn hose-pressure-midpoint [hose clamped pressure]
  (let [segments (:segments hose)
        move-segment
        (fn [[{a-base :base}
             {b-base :base len :length :as b}
             {c-base :base}]]
          ;; this could be better, basically trying to keep a and c apart
          ;; instead of folding them together into inflection points.
          (let [dist-ac (g/dist a-base c-base)
                midpoint (tm/mix a-base c-base 0.5)
                new-base (->> (if (> dist-ac len) (* 2 pressure) pressure)
                              (tm/mix b-base midpoint)
                              clamped)]
            (assoc b
                   :base new-base
                   :angle (g/angle-between new-base c-base))))]
    (assoc hose :segments
           (concat (take 1 segments)
                   (mapv move-segment (partition 3 1 segments))
                   (take-last 1 segments)))))

(defn target-position []
  (tm/+ (cq/rel-vec 0.5 0.5)
        (v/polar (cq/rel-h 0.4)
                 (/ (q/frame-count) 250))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.8)]
    {:start (cq/rel-vec 0.5 0.15)
     :bounds bounds
     :hose (make-hose 2048 (chain/->KinematicSegment (cq/rel-vec 0.5 0.5) tm/HALF_PI 8)
                      (partial next-point bounds 0.6))}))

(defn update-state [{:keys [start bounds hose] :as state}]
  (let [segments (-> hose :segments)
        first-pos (v/clamp-bounds bounds (tm/mix (:base (first segments)) start 0.0))
        last-pos (->> (tm/mix (chain/segment-endpoint (last segments))
                              (target-position) 0.01)
                      (v/clamp-bounds bounds))]
    (-> state
        (update :hose hose-pressure-midpoint (partial v/clamp-bounds bounds) 0.02)
        (update :hose chain/chain-update first-pos last-pos))))

(defn draw [{:keys [hose]}]
  (q/background 1.0 0.2)
  (q/no-fill)
  (cq/draw-path (g/vertices hose)))

(sketch/defquil garden-hose
  :created-at "2021-09-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

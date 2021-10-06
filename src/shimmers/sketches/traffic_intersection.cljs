(ns shimmers.sketches.traffic-intersection
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.utils :as gu]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defrecord Agent [position size velocity max-velocity destination])

(defn overlap? [agent {:keys [position size]}]
  (< (geom/dist (:position agent) position) (+ size (:size agent))))

(defn add-agent [agents]
  (let [pos (tm/random 0.4 0.6)
        tgt (tm/random 0.4 0.6)
        vel (tm/random 0.9 1.5)
        [position velocity destination]
        (rand-nth [[(cq/rel-vec 0.0 pos) (gv/vec2 vel 0) (cq/rel-vec 1.0 tgt)]
                   [(cq/rel-vec 1.0 pos) (gv/vec2 (- vel) 0) (cq/rel-vec 0.0 tgt)]
                   [(cq/rel-vec pos 0.0) (gv/vec2 0 vel) (cq/rel-vec tgt 1.0)]
                   [(cq/rel-vec pos 1.0) (gv/vec2 0 (- vel)) (cq/rel-vec tgt 0.0)]])
        agent (->Agent position 5.0 velocity vel destination)]
    (when-not (some (partial overlap? agent) agents)
      agent)))

(defn add-agents [agents]
  (if-let [agent (add-agent agents)]
    (conj agents agent)
    agents))

;; TODO: implement collision avoidance
;; https://medium.com/@knave/collision-avoidance-the-math-1f6cdf383b5c
;; https://gamedevelopment.tutsplus.com/tutorials/understanding-steering-behaviors-collision-avoidance--gamedev-7777
;; https://gamedevelopment.tutsplus.com/tutorials/understanding-steering-behaviors-seek--gamedev-849
(def max-agents 100)
(def max-force 3.0)
(def mass 10)

;; TODO add avoid for barriers
(defn avoid [{:keys [position velocity size]} nearby obstacles]
  (let [ahead (tm/+ position (tm/* velocity 2))
        [obstacle-pt _]
        (gu/closest-point-on-segments ahead (mapcat geom/edges obstacles))
        closest-agent
        (apply min-key
               (fn [{pos :position}] (geom/dist-squared ahead pos))
               nearby)
        closest (if closest-agent
                  (min-key (partial geom/dist-squared ahead) obstacle-pt (:position closest-agent))
                  obstacle-pt)
        min-dist-sqr (* size size)
        dist-sqr (geom/dist-squared ahead closest)]
    (tm/normalize (tm/- ahead closest) (* max-force (tm/clamp01 (/ min-dist-sqr dist-sqr))))))

(defn steering [{:keys [position velocity destination max-velocity] :as agent} nearby obstacles]
  (let [seek (tm/normalize (tm/- destination position) max-velocity)
        avoid (avoid agent nearby obstacles)
        goal (tm/+ seek avoid)
        steering (tm/div (tm/limit goal max-force) mass)]
    (assoc agent :velocity (tm/limit (tm/+ velocity steering) max-velocity))))

(defn predict [agents bounds obstacles]
  (let [tree (reduce (fn [q agent] (geom/add-point q (:position agent) agent))
                     (spatialtree/quadtree bounds) agents)]
    (for [{:keys [position size] :as agent} agents]
      (let [nearby (spatialtree/select-with-circle tree position (* 2 size))]
        (steering agent (remove #{agent} nearby) obstacles)))))

(defn outside? [bounds {:keys [position]}]
  (not (geom/contains-point? bounds position)))

(defn move [agents bounds]
  (->> agents
       (map (fn [{:keys [velocity] :as agent}]
              (update agent :position tm/+ velocity)))
       (remove (partial outside? bounds))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:obstacles [(rect/rect (cq/rel-vec 0.0 0.0) (cq/rel-vec 0.33 0.33))
               (rect/rect (cq/rel-vec 0.66 0.0) (cq/rel-vec 1.0 0.33))
               (rect/rect (cq/rel-vec 0.0 0.66) (cq/rel-vec 0.33 1.0))
               (rect/rect (cq/rel-vec 0.66 0.66) (cq/rel-vec 1.0 1.0))]
   :bounds (rect/rect (cq/rel-vec 0.0 0.0) (cq/rel-vec 1.0 1.0))
   :agents []})

(defn update-state [{:keys [agents bounds obstacles] :as state}]
  (cond-> state
    (and (< (count agents) max-agents) (p/chance 0.15))
    (update :agents add-agents)
    :always
    (update :agents predict bounds obstacles)
    :always
    (update :agents move bounds)))

(defn draw [{:keys [obstacles agents]}]
  (q/background 1.0 0.25)
  (q/fill 0.8)
  (q/no-stroke)
  (doseq [obstacle obstacles]
    (cq/draw-shape (geom/vertices obstacle)))
  (q/no-fill)

  (doseq [{:keys [position size velocity]} agents]
    (q/stroke-weight 0.5)
    (q/stroke 0.0 1.0)
    (q/no-fill)
    (cq/circle position size)
    (q/stroke 0.3 0.8 0.5 1.0)
    (q/stroke-weight 2.0)
    (q/line position (tm/+ position (tm/* velocity size)))))

(sketch/defquil traffic-intersection
  :created-at "2021-10-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.traffic-intersection
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defrecord Agent [position size velocity])

(defn overlap? [agent {:keys [position size]}]
  (< (geom/dist (:position agent) position) (+ size (:size agent))))

(defn add-agent [agents]
  (let [pos (tm/random 0.4 0.6)
        vel (tm/random 0.8 1.2)
        [position velocity] (rand-nth [[(cq/rel-vec 0.0 pos) (gv/vec2 vel 0)]
                                       [(cq/rel-vec 1.0 pos) (gv/vec2 (- vel) 0)]
                                       [(cq/rel-vec pos 0.0) (gv/vec2 0 vel)]
                                       [(cq/rel-vec pos 1.0) (gv/vec2 0 (- vel))]])
        agent (->Agent position 5.0 velocity)]
    (when-not (some (partial overlap? agent) agents)
      agent)))

(defn add-agents [agents]
  (if-let [agent (add-agent agents)]
    (conj agents agent)
    agents))

(defn adjust [agent nearby]
  agent)

(defn predict [agents bounds]
  (let [tree (reduce (fn [q agent] (geom/add-point q (:position agent) agent))
                     (spatialtree/quadtree bounds) agents)]
    (for [{:keys [position size] :as agent} agents]
      (let [nearby (spatialtree/select-with-circle tree position (* 6 size))]
        (adjust agent nearby)))))

(defn outside? [bounds {:keys [position]}]
  (not (geom/contains-point? bounds position)))

(defn move [agents bounds]
  (->> agents
       (map (fn [{:keys [velocity] :as agent}]
              (update agent :position tm/+ velocity)))
       (remove (partial outside? bounds))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:exclusions [(rect/rect (cq/rel-vec 0.0 0.0) (cq/rel-vec 0.33 0.33))
                (rect/rect (cq/rel-vec 0.66 0.0) (cq/rel-vec 1.0 0.33))
                (rect/rect (cq/rel-vec 0.0 0.66) (cq/rel-vec 0.33 1.0))
                (rect/rect (cq/rel-vec 0.66 0.66) (cq/rel-vec 1.0 1.0))]
   :bounds (rect/rect (cq/rel-vec 0.0 0.0) (cq/rel-vec 1.0 1.0))
   :agents []})

(defn update-state [{:keys [agents bounds] :as state}]
  (cond-> state
    (< (count agents) 10)
    (update :agents add-agents)
    :always
    (update :agents predict bounds)
    :always
    (update :agents move bounds)))

(defn draw [{:keys [exclusions agents]}]
  (q/background 1.0 0.25)
  (q/fill 0.8)
  (q/no-stroke)
  (doseq [boundary exclusions]
    (cq/draw-shape (geom/vertices boundary)))
  (q/no-fill)

  (doseq [{:keys [position size velocity]} agents]
    (q/stroke-weight 0.5)
    (q/stroke 0.0 1.0)
    (q/no-fill)
    (cq/circle position size)
    (q/stroke 0.3 0.8 0.5 1.0)
    (q/stroke-weight 2.0)
    (q/line position (tm/+ position (tm/* velocity 5.0)))))

(sketch/defquil traffic-intersection
  :created-at "2021-10-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

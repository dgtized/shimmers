(ns shimmers.sketches.traffic-intersection
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.core :as sm]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.utils :as gu]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; https://medium.com/@knave/collision-avoidance-the-math-1f6cdf383b5c
;; https://gamedevelopment.tutsplus.com/tutorials/understanding-steering-behaviors-collision-avoidance--gamedev-7777
;; https://gamedevelopment.tutsplus.com/tutorials/understanding-steering-behaviors-seek--gamedev-849
(def max-agents 128)
(def max-force 4.0)
(def mass 8)
(def search-dist (* 3 mass))
(def min-force-dist (* 2 mass))

(defonce ui-state (ctrl/state {:align-width 0.8}))

(defrecord Agent [position size velocity max-velocity destination])

(defn overlap? [agent {:keys [position size]}]
  (< (g/dist (:position agent) position) (+ size (:size agent))))

(defn random-exit
  "Pick a random exit edge not on the starting same edge as the start."
  [bounds start target]
  (let [[p q] (->> bounds
                   g/edges
                   (remove (fn [[p q]] (tm/delta= (+ (g/dist start p) (g/dist start q))
                                                 (g/dist p q))))
                   rand-nth)]
    (tm/mix p q target)))

(defn add-agent [agents bounds]
  (let [pos (tm/random 0.4 0.6)
        tgt (tm/random 0.4 0.6)
        vel (tm/random 0.9 1.5)
        [position velocity]
        (rand-nth [[(cq/rel-vec 0.0 pos) (gv/vec2 vel 0)]
                   [(cq/rel-vec 1.0 pos) (gv/vec2 (- vel) 0)]
                   [(cq/rel-vec pos 0.0) (gv/vec2 0 vel)]
                   [(cq/rel-vec pos 1.0) (gv/vec2 0 (- vel))]])
        destination (random-exit bounds position tgt)
        agent (->Agent position mass velocity vel destination)]
    (when-not (some (partial overlap? agent) agents)
      agent)))

(defn add-agents [agents bounds]
  (if-let [agent (add-agent agents bounds)]
    (conj agents agent)
    agents))

(defn closest-agent [position agents]
  (apply min-key
         (fn [{pos :position}] (g/dist-squared position pos))
         agents))

;; TODO add avoid for barriers
(defn avoid [{:keys [position]} nearby obstacles]
  (let [[obstacle-pt _]
        (gu/closest-point-on-segments position (mapcat g/edges obstacles))
        closest-agent (closest-agent position nearby)
        closest (if closest-agent
                  (min-key (partial g/dist-squared position) obstacle-pt (:position closest-agent))
                  obstacle-pt)
        dist-sqr (g/dist-squared position closest)
        dist-scale (tm/clamp01 (/ (* min-force-dist min-force-dist) dist-sqr))]
    (tm/normalize (tm/- position closest) (* max-force dist-scale))))

(defn align
  "Try aligning with any agents with a similar heading."
  [{:keys [velocity]} nearby align-width]
  (let [heading (g/heading velocity)

        common-heading
        (filter (fn [{:keys [velocity]}]
                  (< (sm/radial-distance (g/heading velocity) heading)
                     align-width))
                nearby)]
    (if (empty? common-heading)
      (gv/vec2)
      (tm/div (reduce tm/+ velocity (map :velocity common-heading))
              (inc (count common-heading))))))

(defn steering [{:keys [position velocity destination max-velocity] :as agent} nearby obstacles]
  (let [state @ui-state
        width (:align-width state)

        seek (tm/normalize (tm/- destination position) max-velocity)
        avoid (avoid agent nearby obstacles)
        align (align agent nearby width)
        goal (tm/+ seek avoid align)
        steering (tm/div (tm/limit goal max-force) mass)]
    (assoc agent :velocity (tm/limit (tm/+ velocity steering) max-velocity))))

(defn predict [agents bounds obstacles]
  (let [tree (reduce (fn [q agent] (g/add-point q (:position agent) agent))
                     (spatialtree/quadtree bounds) agents)]
    (for [{:keys [position] :as agent} agents]
      (let [nearby (spatialtree/select-with-circle tree position search-dist)]
        (steering agent (remove #{agent} nearby) obstacles)))))

(defn outside? [bounds {:keys [position]}]
  (not (g/contains-point? bounds position)))

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
    (and (< (count agents) max-agents) (p/chance 0.33))
    (update :agents add-agents bounds)
    :always
    (update :agents predict bounds obstacles)
    :always
    (update :agents move bounds)))

(defn draw [{:keys [obstacles agents]}]
  (q/background 1.0 0.4)
  (q/fill 0.8)
  (q/no-stroke)
  (doseq [obstacle obstacles]
    (cq/draw-polygon obstacle))
  (q/no-fill)

  (doseq [{:keys [position size velocity]} agents]
    (q/stroke-weight 0.5)
    (q/stroke 0.0 1.0)
    (q/no-fill)
    (cq/circle position size)
    (q/stroke 0.3 0.8 0.5 1.0)
    (q/stroke-weight 2.0)
    (q/line position (tm/+ position (tm/* velocity size)))))

(defn ui-controls []
  [:div (ctrl/slider ui-state (fn [v] (str "Alignment Width " v))
                     [:align-width] [0.0 Math/PI 0.1])])

(sketch/defquil traffic-intersection
  :created-at "2021-10-05"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

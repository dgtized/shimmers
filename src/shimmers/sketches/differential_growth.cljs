(ns shimmers.sketches.differential-growth
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn path-split [threshold points]
  (concat (apply concat
                 (for [[p q] (partition 2 1 points)]
                   (if (or (> (g/dist p q) threshold)
                           (dr/chance 0.0005))
                     [p (tm/+ (tm/mix p q 0.5) (dr/jitter 10))]
                     [p])))
          [(last points)]))

(defn rejection-force [{:keys [neighborhood repulsion]} from points]
  (let [forces (->> points
                    (filter (fn [p] (and (not= from p) (< (g/dist from p) neighborhood))))
                    (map (fn [p] (tm/* (tm/- from p) (/ repulsion (g/dist from p))))))]
    (if (empty? forces)
      (gv/vec2)
      (reduce tm/+ forces))))

(defn apply-forces [{:keys [attraction alignment] :as config} points]
  (concat [(first points)]
          (for [[a b c] (partition 3 1 points)]
            (reduce tm/+ b
                    [(tm/* (tm/- a b) attraction)
                     (tm/* (tm/- c b) attraction)
                     (tm/* (tm/- (tm/mix a c 0.5) b) alignment)
                     (rejection-force config b points)]))
          [(last points)]))

(defn path-update [{:keys [points]}]
  (let [config {:attraction 0.05 :alignment 0.05
                :neighborhood (cq/rel-w 0.05)
                :repulsion 0.75}]
    (->> points
         (path-split (cq/rel-w 0.05))
         (apply-forces config)
         gl/linestrip2)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:path (gl/linestrip2 [(cq/rel-vec 0.05 0.5) (cq/rel-vec 0.95 0.5)])})

(defn update-state [state]
  (update state :path path-update))

(defn draw-path [vertices]
  (q/no-fill)
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape)
  (q/fill 0.0)
  (doseq [v vertices]
    (cq/circle v 3)))

(defn draw [{:keys [path]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (draw-path (:points path)))

(sketch/defquil differential-growth
  :created-at "2022-02-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

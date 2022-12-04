(ns shimmers.sketches.chaos-cuts
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.line :as gl]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [thi.ng.geom.core :as g]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.8)]
    {:bounds bounds
     :shapes []
     :lines [(gl/line2 (cq/rel-vec 0 0) (cq/rel-vec 1 1))]}))

(defn cut [line shape]
  (->> (lines/cut-polygon shape line)
       (mapcat (fn [poly]
                 (->> (poly-detect/inset-polygon poly 4)
                      poly-detect/split-self-intersection
                      (filter (fn [inset] (> (g/area inset) 100))))))))

(defn update-state [{:keys [bounds lines] :as state}]
  (assoc state :shapes
         (reduce (fn [polygons line]
                   (mapcat (fn [poly] (cut line poly)) polygons))
                 [bounds]
                 lines)))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/fill 1.0)
  (doseq [polygon shapes]
    (cq/draw-polygon polygon)))

(sketch/defquil chaos-cuts
  :created-at "2022-12-04"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

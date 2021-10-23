(ns shimmers.sketches.intertwined
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]))

;; Random path through a space, then subdivide into polygons where the path crosses itself

;; this is *almost* working but sometimes shows intersects outside of segment
(defn intersections [path]
  (loop [intersections [] segments (partition 2 1 path)]
    (if (empty? segments)
      intersections
      (let [[current & xs] segments
            hits (keep (partial geometry/segment-intersect current) (rest xs))]
        (recur (into intersections hits) xs)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [b (cq/screen-rect 0.95)
        zones (g/subdivide b {:rows 5 :cols 5})
        k (* 0.2 (count zones))
        zones (cons (first zones) (drop k (shuffle (rest zones))))]
    {:path (map g/centroid zones)}))

(defn update-state [state]
  state)

(defn draw [{:keys [path]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (cq/draw-path path)
  (let [intersects (intersections path)
        isecs (count intersects)]
    (doseq [[idx p] (map-indexed vector intersects)]
      (q/fill (/ idx isecs) 0.75 0.6)
      (cq/circle p 10.0))))

(sketch/defquil intertwined
  :created-at "2021-10-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

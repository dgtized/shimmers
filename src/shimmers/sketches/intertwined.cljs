(ns shimmers.sketches.intertwined
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]))

;; Random path through a space, then subdivide into polygons where the path crosses itself

(defn intersect-point
  "Return point of intersection between two lines or nil."
  [l1 l2]
  (when-let [{:keys [type] :as hit} (g/intersect-line l1 l2)]
    (when (= type :intersect)
      (:p hit))))

(defn intersections [path]
  (loop [intersections []
         segments (map gl/line2 (partition 2 1 path))]
    (if (empty? segments)
      intersections
      (let [[current & xs] segments
            hits (keep (partial intersect-point current) (rest xs))
            {[p _] :points} current
            ;; order points as distance along path
            ordered-hits (sort-by (fn [c] (g/dist p c)) hits)]
        (recur (into intersections ordered-hits) xs)))))

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
      (cq/circle p (+ 6 (* 14 (- 1.0 (/ idx isecs))))))))

(sketch/defquil intertwined
  :created-at "2021-10-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

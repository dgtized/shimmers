(ns shimmers.sketches.spaces-divided
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn gen-line [bounds]
  (fn []
    (let [[a b c d] (g/edges bounds)
          [[p1 q1] [p2 q2]] (dr/rand-nth [[a c] [b d]])]
      (gl/line2 (tm/mix p1 q1 (dr/random 0.1 0.9))
                (tm/mix p2 q2 (dr/random 0.1 0.9))))))

(defn isec-point [l1 l2]
  (when-let [{:keys [type] :as hit} (g/intersect-line l1 l2)]
    (when (= type :intersect)
      (:p hit))))

(defn line-intersections [lines]
  (remove nil?
          (for [[a b] (cs/all-pairs lines)]
            (isec-point a b))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :lines (repeatedly 6 (gen-line bounds))}))

(defn update-state [{:keys [lines] :as state}]
  (assoc state :intersections (line-intersections lines)))

(defn draw [{:keys [lines intersections]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (doseq [{[p q] :points} lines]
    (q/line p q))

  (swap! defo assoc :isecs intersections)
  (doseq [p intersections]
    (cq/circle p 3.0)))

(sketch/defquil spaces-divided
  :created-at "2021-12-09"
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

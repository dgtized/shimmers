(ns shimmers.sketches.lattice-in-steps
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[a b c] (map #(tm/+ % (cq/rel-pos 0.5 0.5))
                     [(gv/vec2 15 0) (gv/vec2 -15 -15) (gv/vec2 -15 15)])]
    {:nodes [a b c]
     :edges [[a b] [b c] [c a]]}))

;; Not quite working, idea was to keep adding triangles on each outside face,
;; but somehow this is also generating polygons. Maybe from the discontinuity at
;; the wrap-around for the hull?
;; Never mind, discontunity is from convex hull skipping "outer" points. Need a "shrink" operation?
(defn update-state [{:keys [nodes] :as state}]
  (let [hull (gp/convex-hull* nodes)
        [p q] (rand-nth (partition 2 1 hull))
        m (->> #(geometry/confused-midpoint p q 0.9)
               (repeatedly 20)
               (remove (fn [c] (g/contains-point? (gp/polygon2 hull) c)))
               (filter (fn [c] (g/contains-point? (cq/screen-rect) c)))
               first)]
    (if m
      (-> state
          (update :nodes conj m)
          (update :edges into [[p m] [m q]]))
      state)))

(defn draw [{:keys [nodes edges]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [node nodes]
    (cq/circle node 0.5))
  (doseq [[p q] edges]
    (q/line p q)))

(sketch/defquil lattice-in-steps
  :created-at "2021-05-29"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

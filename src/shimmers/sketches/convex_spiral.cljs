(ns shimmers.sketches.convex-spiral
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.polygon :as gp]))

;; Concept:
;; Draw a convex hull, remove points on hull, recurse until 3 points, connecting
;; end of outer with start of inner.

(defn generate-points [n dist]
  (repeatedly n #(gv/vec2 (dist) (dist))))

;; FIXME: sometimes there is overlap of an outer path and an inner?
;; Might just be at transition from outer to inner hull?
;; also is there a way to handle the last 3 to continue spiral?
(defn convex-spiral [points]
  (loop [path [] points points]
    (if (< (count points) 3)
      (into path points)
      (let [hull (gp/convex-hull* points)]
        (recur (into path hull)
               (remove (set hull) points))))))

(defn setup []
  (q/no-loop)
  {:points (generate-points 64 #(q/random 0.15 0.85))})

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/background 255)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (q/fill 0 0 0)
  (doseq [p points
          :let [[x y] (cq/rel-pos p)]]
    (q/ellipse x y 1 1))

  (doseq [[p q] (partition 2 1 (convex-spiral (map cq/rel-pos points)))]
    (q/line p q)))

(defn ^:export run-sketch []
  ;; 20210322
  (q/defsketch convex-spiral-sketch
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

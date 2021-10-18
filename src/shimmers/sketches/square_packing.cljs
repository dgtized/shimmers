(ns shimmers.sketches.square-packing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.probability :as p]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.math.core :as tm]))

;; Further Experiments: pack resulting squares with patterns of their own?
;; Colors and shapes, even tilted or "hand drawn" squares?

(defn middle-out
  "Alternate distribution for px,py"
  []
  (p/weighted {0.0 1.0
               0.5 1.0
               1.0 1.0}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:squares []
   :remaining [(cq/screen-rect 0.98)]})

(defn random-ratio []
  (p/weighted {(/ 1 tm/PHI) 4
               0.5 2
               (/ 1 3) 2}))

(defn update-state [{:keys [remaining squares] :as state}]
  (if (and (not-empty remaining) (< (count squares) 256))
    (let [rect (p/weighted-by geom/area remaining)
          percent (repeatedly 2 (fn [] (mod (* tm/PHI (rand)) 1.0)))
          [s & r] (square/proportional-split rect (/ 1 tm/PHI) percent)]
      (-> state
          (assoc :remaining (into (remove #{rect} remaining) r))
          (update :squares conj s)))
    state))

(defn draw [{:keys [squares remaining]}]
  (q/background 1.0)
  (q/stroke-weight 0.66)
  (q/stroke 0.35 0.5 0.5 0.5)
  (q/fill 1.0 1.0)
  (doseq [rects remaining]
    (cq/draw-shape (geom/vertices rects)))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/fill 1.0 0.1)
  (doseq [square squares]
    (cq/draw-shape (geom/vertices (geom/scale-size square (/ 1 tm/PHI))))))

(sketch/defquil square-packing
  :created-at "2021-10-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

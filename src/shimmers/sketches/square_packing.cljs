(ns shimmers.sketches.square-packing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

;; Further Experiments: pack resulting squares with patterns of their own?
;; Colors and shapes, even tilted or "hand drawn" squares?

(defn middle-out
  "Alternate distribution for px,py"
  []
  (dr/weighted {0.0 1.0
                0.5 1.0
                1.0 1.0}))

(defn random-ratio []
  (dr/weighted {(/ 1 tm/PHI) 4
                0.5 2
                (/ 1 3) 2}))

(defn pack-step
  [{:keys [remaining squares square-limit pick-rectangle ratio position] :as state}]
  (if (and (not-empty remaining) (< (count squares) square-limit))
    (let [rect (pick-rectangle remaining)
          [square & panes]
          (square/proportional-split rect (ratio rect) (position rect))]
      (-> state
          (assoc :remaining (into (remove #{rect} remaining) panes))
          (update :squares conj square)))
    state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:square-limit 256
   :pick-rectangle (partial dr/weighted-by g/area)
   :position #(repeatedly 2 (fn [] (mod (* tm/PHI (dr/random)) 1.0)))
   :ratio (constantly (/ 1 tm/PHI))

   :squares []
   :remaining [(cq/screen-rect 0.98)]})

(defn update-state [state]
  (pack-step state))

(defn draw [{:keys [squares remaining]}]
  (q/background 1.0)
  (q/stroke-weight 0.66)
  (q/stroke 0.35 0.5 0.5 0.5)
  (q/fill 1.0 1.0)
  (doseq [rects remaining]
    (cq/draw-shape (g/vertices rects)))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/fill 1.0 0.1)
  (doseq [square squares]
    (-> square
        (g/scale-size (/ 1 tm/PHI))
        g/vertices
        cq/draw-shape)))

(sketch/defquil square-packing
  :created-at "2021-10-17"
  :tags #{:deterministic}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

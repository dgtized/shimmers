(ns shimmers.sketches.dreamcatcher
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn next-row [row decay]
  (map (fn [[a b]] [(tm/mix (tm/mix a b 0.5) (gv/vec2) decay) b])
       (partition 2 1 (conj row (first row)))))

(defn dreamloop [{:keys [points row limit decay] :as state}]
  (if (< (count points) limit)
    (let [added-row (next-row row decay)]
      (recur (-> state
                 (update :points concat
                         (cons (last (last added-row)) (mapcat identity added-row)))
                 (assoc :row (mapv first added-row)))))
    state))

(defn update-state []
  (let [shape (gc/circle (cq/rel-h 0.48))
        points (g/vertices shape 16)
        fc (q/frame-count)]
    (dreamloop {:points points
                :row points
                :limit 1000
                :rotation (* eq/TAU (math/sin (/ fc 400)))
                :decay (+ 0.08 (* 0.05 (math/sin (/ fc 160))))})))

(defn draw [{:keys [points rotation]}]
  (q/background 1.0)
  (q/no-fill)
  (q/with-translation (cq/rel-vec 0.5 0.5)
    (q/with-rotation [rotation]
      (cq/draw-path points))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition dreamcatcher
  {:created-at "2021-10-11"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

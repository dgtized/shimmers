(ns shimmers.sketches.intersecting-chords
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-circle [{p :p [w h] :size}]
  (let [r (dr/gaussian 30 10)]
    (gc/circle (tm/+ p (gv/vec2 (dr/random r (- w r))
                                (dr/random r (- h r))))
               r)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.95)
        circles (repeatedly 32 #(make-circle bounds))]
    {:bounds bounds
     :circletree (reduce (fn [t c] (saq/add-point t (:p c) c))
                         (saq/circletree bounds)
                         circles)}))

(defn update-state [state]
  state)

(defn draw [{:keys [bounds circletree]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (doseq [c (saq/all-data circletree)]
    (cq/circle c)
    (doseq [nearby (saq/k-nearest-neighbors circletree 3 (:p c))]
      (when-let [neighbor (g/get-point-data nearby)]
        (q/line (:p c) (:p neighbor))))))

(sketch/defquil intersecting-chords
  :created-at "2023-01-04"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

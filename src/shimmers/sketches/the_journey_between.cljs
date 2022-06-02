(ns shimmers.sketches.the-journey-between
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn build-grid [seed grid-size]
  (for [loc (g/subdivide (cq/screen-rect) grid-size)
        :let [noise (dr/noise-at-point seed 0.01 (g/centroid loc))]]
    (assoc loc :noise (- 1.0 noise))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [seed (gv/vec2 (dr/random 100) (dr/random 100))
        grid-size {:cols 40 :rows 30}
        grid (build-grid seed grid-size)]
    {:seed seed
     :grid-size grid-size
     :grid grid}))

(defn update-state [state]
  state)

(defn draw [{:keys [grid]}]
  (q/no-stroke)
  (doseq [{[x y] :p [w h] :size noise :noise} grid]
    (q/fill 0.0 0.0 noise 1.0)
    (q/rect x y w h)))

(sketch/defquil the-journey-between
  :created-at "2022-06-02"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

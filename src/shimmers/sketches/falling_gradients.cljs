(ns shimmers.sketches.falling-gradients
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/noise-seed (dr/seed))
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {})

(defn discrete-curve [slices phase scale offset]
  (for [x (tm/norm-range slices)]
    [x (* scale (q/noise (* x phase) offset))]))

(defn random-triangle-at [pos rotation scale]
  (triangle/inscribed-equilateral {:p pos :r scale} rotation))

(defn draw []
  (q/background 1.0)
  (q/fill 0.2 0.008)
  (q/no-stroke)
  (let [slices (dr/random-int 64 192)
        curve (discrete-curve slices 2 0.4 1000)
        depth-curve (map second (discrete-curve slices 5 1.0 50000))
        slice-width (cq/rel-w (/ 1.0 slices))]
    (doseq [[[x1 y1] depth] (map vector curve depth-curve)
            :let [theta (dr/random-tau)]]
      (let [f (dr/random -0.0075 -0.0125)]
        (doseq [s (range 400)
                :let [d (* depth (math/exp (* f s)))]]
          (-> (cq/rel-vec x1 (+ y1 d))
              (random-triangle-at (+ theta (* eq/TAU d))
                                  slice-width)
              cq/draw-polygon))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [900 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p.readable-width
    "Layer triangles as they slowly rotate and fall to a specific depth."]])

(sketch/definition falling-gradients
  {:created-at "2021-05-04"
   :tags #{:static :deterministic}
   :type :quil}
  (ctrl/mount page))

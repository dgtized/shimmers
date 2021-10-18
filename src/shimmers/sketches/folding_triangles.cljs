(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn unfurled [triangle]
  (let [[a b c] (:points triangle)]
    (gt/triangle3 (geometry/reflect-over-edge c [a b])
                  (geometry/reflect-over-edge b [a c])
                  (geometry/reflect-over-edge a [b c]))))

(defn draw [_]
  (q/background 255)
  (let [depth 10
        theta (mod (/ (q/millis) 500) (* depth 6 Math/PI))
        base (-> (gt/equilateral2 1 1.5)
                 (g/center (gv/vec3))
                 (g/rotate (/ theta 12)))
        all
        (mapcat (fn [triangle i]
                  (let [start (* Math/PI i)
                        end (+ start (* (- depth i) 6 Math/PI))]
                    (if (and (> theta start) (< theta end))
                      (map (fn [edge]
                             (assoc (geometry/rotate-over-edge triangle edge (- theta start))
                                    :color (mod (- (* 0.1 i) 0.5) 1.0)))
                           (g/edges triangle))
                      [])))
                (take depth (iterate unfurled base))
                (take depth (iterate inc 0)))]
    (q/scale 3)
    (doseq [t all]
      (q/fill (:color t) 0.8 0.5 0.1)
      (cq/draw-shape (g/vertices t)))))

(sketch/defquil folding-triangles
  :created-at "2021-02-28"
  :size [900 600]
  :renderer :p3d
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

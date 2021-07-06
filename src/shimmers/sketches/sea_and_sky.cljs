(ns shimmers.sketches.sea-and-sky
  "More triangle fill techniques but trying to evoke Rothko a little?"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn random-triangle-at [pos rotation scale]
  (-> (gt/triangle2 [0 0] [0.2 0.8] [1.0 0.1])
      (geom/rotate rotation)
      (geom/scale-size scale)
      (geom/translate pos)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:sea (rect/rect (cq/rel-pos 0.02 0.4) (cq/rel-pos 0.98 0.98))
   :sky (rect/rect (cq/rel-pos 0.02 0.02) (cq/rel-pos 0.98 0.35))})

(defn update-state [state]
  state)

(defn draw [{:keys [sea sky]}]
  (q/no-stroke)
  (q/ellipse-mode :radius)
  (q/fill (+ 0.6 (* 0.04 (q/random-gaussian))) (tm/random 0.4 0.6) (tm/random 0.4 0.75) 0.05)
  (let [angle (* 0.05 (q/random-gaussian))
        sea' (geometry/rotate-around-centroid sea angle)]
    (dotimes [i 96]
      (let [pos (geom/random-point-inside sea')]
        (if (p/chance 0.1)
          (let [theta (tm/random tm/TWO_PI)
                size (tm/random 6 16)
                d (rand-nth [-0.2 0.2])]
            (dotimes [j (int (tm/random 8 32))]
              (-> (tm/+ pos (v/polar (* j d size) angle))
                  (random-triangle-at theta size)
                  geom/vertices
                  cq/draw-shape)))
          (-> pos
              (random-triangle-at (tm/random tm/TWO_PI) (tm/random 6 32))
              geom/vertices
              cq/draw-shape)))))
  (q/fill (+ 0.02 (* 0.05 (q/random-gaussian)))
          (tm/random 0.4 0.6)
          (tm/random 0.4 0.75)
          0.05)

  (dotimes [i 32]
    (let [pos (geom/random-point-inside sky)]
      (if (p/chance 0.1)
        (let [r (tm/random 1.0 10.0)
              d (rand-nth [-0.5 0.5])]
          (dotimes [j (int (tm/random 8 32))]
            (cq/circle (tm/+ pos (gv/vec2 (* d r j) 0)) r)))
        (cq/circle pos (tm/random 1.0 10.0))))))

(sketch/defquil sea-and-sky
  :created-at "2021-07-06"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

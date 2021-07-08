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
            [thi.ng.math.core :as tm]))

(defn random-triangle-at [pos rotation scale]
  (geometry/shape-at (gt/triangle2 [0 0] [0.2 0.8] [1.0 0.1])
                     rotation scale pos))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:sea (rect/rect (cq/rel-pos 0.02 0.4) (cq/rel-pos 0.98 0.98))
   :sky (rect/rect (cq/rel-pos 0.02 0.02) (cq/rel-pos 0.98 0.35))})

(defn wave [rotation size pos]
  (-> pos
      (random-triangle-at rotation size)
      geom/vertices
      cq/draw-shape))

(defn streak [pos length step-size angle draw]
  (dotimes [j length]
    (draw (tm/+ pos (v/polar (* j step-size) angle)))))

(defn draw [{:keys [sea sky]}]
  (q/no-stroke)
  (q/ellipse-mode :radius)
  (q/fill (+ 0.6 (* 0.04 (q/random-gaussian))) (tm/random 0.4 0.6) (tm/random 0.4 0.75) 0.05)
  (let [angle (* 0.05 (q/random-gaussian))
        sea' (geometry/rotate-around-centroid sea angle)]
    (dotimes [_ 64]
      (let [pos (geom/random-point-inside sea')]
        (if (p/chance 0.2)
          (let [theta (tm/random tm/TWO_PI)
                size (tm/random 6 12)
                d (rand-nth [-0.2 0.2])]
            (streak pos (int (tm/random 8 32)) (* d size) angle
                    #(wave theta size %)))
          (wave (tm/random tm/TWO_PI) (tm/random 6 20) pos))))

    ;; Add whitecaps
    (q/fill 0.5 (tm/random 0.9 1.0) (tm/random 0.9 1.0) 0.11)
    (dotimes [_ 2]
      (let [pos (geom/random-point-inside sea')]
        (if (p/chance 0.4)
          (let [theta (tm/random tm/TWO_PI)
                size (tm/random 4 10)
                d (rand-nth [-0.3 0.3])]
            (streak pos (int (tm/random 8 40)) (* d size) (+ angle (tm/random -0.08 0.08))
                    #(wave theta size %)))
          (wave (tm/random tm/TWO_PI) (tm/random 6 16) pos)))))

  (q/fill (+ 0.02 (* 0.05 (q/random-gaussian)))
          (tm/random 0.4 0.6)
          (tm/random 0.4 0.75)
          0.05)
  (dotimes [_ 32]
    (let [pos (geom/random-point-inside sky)]
      (if (p/chance 0.1)
        (let [r (tm/random 1.0 10.0)
              d (rand-nth [-0.5 0.5])]
          (streak pos (int (tm/random 8 32)) (* d r) 0 #(cq/circle % r)))
        (cq/circle pos (tm/random 1.0 10.0))))))

(sketch/defquil sea-and-sky
  :created-at "2021-07-06"
  :size [800 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])

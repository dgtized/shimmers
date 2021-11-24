(ns shimmers.sketches.clothoid-flowers
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn plot [scale points]
  (q/begin-shape)
  (doseq [[x y] points]
    (cq/circle (* scale x) (* scale y) 0.1))
  (q/end-shape))

(defn draw [{:keys [t]}]
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke 0.0 0.1)
  (q/stroke-weight 0.8)

  (q/translate (cq/rel-vec 0.5 0.5))
  (let [rotation (* 0.1 t)
        length (+ 40 (* 20 (Math/sin t)))]
    (plot 12 (eq/clothoid 18 length 30 -1 (+ rotation 0.0) (gv/vec2)))
    (plot 12 (eq/clothoid 12 length 50 -1 (+ rotation Math/PI) (gv/vec2)))))

(sketch/defquil clothoid-flowers
  :created-at "2021-11-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

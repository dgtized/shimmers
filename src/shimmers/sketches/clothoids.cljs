(ns shimmers.sketches.clothoids
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.005))

;; TODO: use a Cornu or clothoid spiral
;; https://pwayblog.com/2016/07/03/the-clothoid/ origin is the midpoint where r
;; is infinity and L is 0, and each side of the curve is one of the two circles.
;; https://math.stackexchange.com/questions/1785816/calculating-coordinates-along-a-clothoid-betwen-2-curves
;; https://etrr.springeropen.com/articles/10.1007/s12544-013-0119-8
(defn plot [points]
  (q/begin-shape)
  (doseq [[x y] points]
    (cq/circle x y 0.1)
    (q/vertex x y))
  (q/end-shape))

(defn pen-color [n]
  (q/stroke (mod (* n tm/PHI) 1.0) 0.7 0.4))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke 0)
  (q/stroke-weight 0.04)

  (q/translate (cq/rel-vec 0.5 0.5))
  (q/scale 11.0)
  (let [length (+ 40 (* 20 (Math/sin t)))
        A3 (+ 15 (* 5 (Math/cos t)))
        A4 (+ 8 (* 2 (Math/cos t)))]
    (pen-color 0)
    (plot (eq/clothoid 18 length 20 -1 0.0 (gv/vec2)))
    (pen-color 1)
    (plot (eq/clothoid 12 length 50 -1 Math/PI (gv/vec2)))
    (pen-color 2)
    (plot (eq/clothoid-from A3 50 30 1 0 (gv/vec2 0.0 0.0)))
    (pen-color 3)
    (plot (eq/clothoid-from A4 30 30 1 Math/PI (gv/vec2 0.0 0.0)))
    ))

(sketch/defquil clothoids
  :created-at "2021-11-23"
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

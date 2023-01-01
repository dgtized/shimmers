(ns shimmers.sketches.unwinding
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.core :as g]))

(defn plot [r points]
  (q/begin-shape)
  (doseq [[x y] points]
    #_(cq/circle x y r)
    (q/vertex x y))
  (q/end-shape))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t (fn [t] (mod (+ t 0.005) eq/TAU))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/translate (cq/rel-vec 0.5 0.5))
  (q/scale 1 -1)
  (q/stroke-weight 3)
  (let [length (+ 40 (* 20 (Math/sin t)))
        length-out (+ 30 (* 15 (Math/sin (+ t 1.0))))
        scaled (fn [s] (fn [p] (tm/* p s)))
        inner (mapv (scaled 7) (eq/clothoid 15 length 75 1 t (gv/vec2)))
        angle (g/heading (apply tm/- (reverse (take-last 2 inner))))
        outer (mapv (comp (fn [p] (g/translate p (last inner)))
                          (scaled 5))
                    (eq/clothoid (+ 10 (* 2 (Math/sin t))) length-out
                                 (int (+ 40 (* 20 (Math/sin t))))
                                 -1 angle (gv/vec2)))
        length-back (+ 40 (* 10 (Math/sin (+ t Math/PI))))
        angle2 (g/heading (apply tm/- (reverse (take-last 2 outer))))
        back (mapv (comp (fn [p] (g/translate p (last outer)))
                         (scaled 4))
                   (eq/clothoid-from 10 length-back 50 1 (- eq/TAU angle2) (gv/vec2)))]
    (doseq [base (butlast (tm/norm-range 5))]
      (q/with-rotation [(* base eq/TAU)]
        (plot 2 inner)
        (plot 2 outer)
        (plot 2 back)))))

(sketch/defquil unwinding
  :created-at "2023-01-01"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

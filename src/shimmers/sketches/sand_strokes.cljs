(ns shimmers.sketches.sand-strokes
  "Attempting the sand-stroke technique from
  http://www.complexification.net/gallery/machines/sandstroke/ and further
  explained in https://inconvergent.net/2017/grains-of-sand/."
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.math.core :as tm]))

(defn displacement-noise [t v]
  (cq/rel-h (tm/mix-exp 0.0001 0.15 (q/noise (/ t 0.5) (* 2 v)) 2)))

(defn rand-color []
  [0 0 0 0.08])

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-detail 6 0.75)
  (q/ellipse-mode :radius)
  {:t 0
   :v 0
   :density 48
   :shape (gl/line2 (cq/rel-pos 0.05 0.5)
                    (cq/rel-pos 0.95 0.5))
   :displacement (displacement-noise 0 0)
   :angle 0
   :color (rand-color)})

(def dt 0.2)

(defn update-state [{:keys [t v color] :as state}]
  (let [t' (mod (+ t (* dt (rand))) 1.0)
        new-pass (< t' t)
        v' (if new-pass (inc v) v)
        color' (if new-pass (rand-color) color)]
    (assoc state
           :t t'
           :v v'
           :color color'
           :angle (ksd/draw (ksd/normal {:mu 0 :sd 1})))))

(defn draw [{:keys [t v density shape angle color]}]
  (q/stroke-weight 0.3)
  (q/no-fill)
  (apply q/stroke color)
  (let [cols 64
        ;; normal (ksd/normal {:mu 0.5 :sd 0.05})
        uniform (ksd/uniform {:a 0.1 :b 0.9})]
    (dotimes [iter cols]
      (let [t (+ t (* dt (+ (/ iter cols) (* (/ 1 cols) (rand)))))
            s-disp (displacement-noise t v)
            [x y] (geom/point-at shape t)
            line (gl/line2 (+ x angle) (- y s-disp) (- x angle) (+ y (* 1.2 s-disp)))]
        (doseq [p (ksd/sample density uniform)
                :let [[x y] (geom/point-at line p)]]
          (q/ellipse x y 0.05 0.05))))))

(defn ^:export run-sketch []
  (q/defsketch sand-strokes
    :host "quil-host"
    :size [800 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

(ns shimmers.sketches.stretchy-curves
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 0.5)
  {:outline (dr/rand-nth [(gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.5))
                          (g/as-polygon (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.5)) 6)
                          (cq/screen-rect 0.9)])
   :weights (repeatedly 2 #(dr/random -0.1 0.1))
   :phase (repeatedly 2 dr/random-tau)
   :time (/ (q/millis) 1000.0)})

(defn update-state [{:keys [time] :as state}]
  (let [dt (- (/ (q/millis) 1000.0) time)]
    (-> state (update :time + dt))))

(defn draw [{[w0 w1] :weights [p0 p1] :phase :keys [time outline]}]
  (q/background 1.0)
  (let [N 256
        center (cq/rel-vec 0.5 0.5)
        t (+ (* 2.5 time) (* 1.5 (math/sin (+ (* 0.35 time) (* 2 (math/sin (* 0.4 time)))))))
        d (+ 0.05 (* 0.2 (eq/unit-sin t)))
        d2 (+ 0.5 (* 0.4 (math/sin (+ (* 0.3 t) (math/sin (* 0.4 t))))))]
    (dotimes [i N]
      (let [a (mod (/ (- i (* 0.070 t) (* 0.15 N (math/sin (+ (* w0 i) (* 0.25 t) p0))))
                      (float N)) 1.0)
            b (- 1.0 (mod (/ (+ i (* 0.011 t) (* 0.25 N (math/sin (- (* w1 i) (* 0.35 t) p1))))
                             (float N)) 1.0))
            [px py] (g/point-at outline a)
            [qx qy] (g/point-at outline b)
            [rx ry] (tm/mix center (g/point-at outline (sm/mix-mod a b d)) d2)
            [sx sy] (tm/mix center (g/point-at outline (sm/mix-mod a b (- 1.0 d))) d2)]
        (q/bezier px py rx ry sx sy qx qy)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition stretchy-curves
  {:created-at "2024-06-01"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

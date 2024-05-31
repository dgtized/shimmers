(ns shimmers.sketches.stretchy-lines
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/stroke-weight 0.5)
  {:outline (dr/rand-nth [(gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.5))
                          (g/as-polygon (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.5)) 6)
                          (cq/screen-rect 0.9)])
   :weights (repeatedly 2 #(dr/random -0.1 0.1))
   :phase (repeatedly 2 dr/random-tau)})

(defn update-state [state]
  state)

(defn draw [{[w0 w1] :weights [p0 p1] :phase :keys [outline]}]
  (q/background 1.0)
  (let [N 256
        secs (/ (q/millis) 1000.0)
        t (+ (* 0.9 secs)
             (math/sin (+ w0 (* 0.75 secs)
                          (* 2 (eq/cube (math/sin (+ p0 (* 0.5 secs))))))))]
    (dotimes [i N]
      (let [a (mod (/ (- i (* 0.09 t) (* 0.6 N (math/sin (+ (* w0 i) (* 0.25 t) p0))))
                      (float N)) 1.0)
            b (mod (/ (+ i (* 0.13 t) (* 0.7 N (math/sin (- (* w1 i) (* 0.35 t) p1))))
                      (float N)) 1.0)]
        (q/line (g/point-at outline a)
                (g/point-at outline b))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition stretchy-lines
  {:created-at "2024-05-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

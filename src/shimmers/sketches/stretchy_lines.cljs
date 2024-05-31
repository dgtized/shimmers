(ns shimmers.sketches.stretchy-lines
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.core :as g]
   [clojure.math :as math]
   [shimmers.math.deterministic-random :as dr]))

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
        t (+ secs (math/sin (+ w0 (* 0.75 secs) (math/sin (+ p0 (* 0.5 secs))))))]
    (dotimes [i N]
      (let [a (mod (/ (- i (* 0.15 t) (* 0.6 N (math/sin (+ (* w0 i) (* 0.3 t) p0))))
                      (float N)) 1.0)
            b (mod (/ (+ i (* 0.10 t) (* 0.7 N (math/sin (- (* w1 i) (* 0.4 t) p1))))
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

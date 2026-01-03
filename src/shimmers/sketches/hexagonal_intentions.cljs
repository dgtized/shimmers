(ns shimmers.sketches.hexagonal-intentions
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (let [t (/ (q/millis) 2000)
        center (cq/rel-vec 0.5 0.5)
        radius (* 0.5 (q/height))
        polygon
        ;; FIXME: can it stay mostly hexagonal for most of the time and then
        ;; fiddle with low frequency modulation on some of the vertices?
        (gp/polygon2
         (mapv (fn [s]
                 (v/+polar center
                           (+ (* 0.33 radius)
                              (* (eq/unit-sin
                                  (+ (* eq/TAU s) (* 0.125 t) (math/sin (- (* 0.05 t) (* eq/TAU s)))))
                                 (* 0.66 radius)))
                           (+ (* s eq/TAU)
                              (math/sin
                               (+ t (math/sin (+ (* eq/TAU s) (* 0.077 t))))))))
               (butlast (tm/norm-range 6))))
        c (+ tm/PHI (math/sin (* 0.02 eq/TAU t)))]
    (q/background 1.0)
    (q/stroke-weight 1.33)
    (qdg/draw polygon)
    (q/stroke-weight 0.66)
    (doseq [s (tm/norm-range 72)]
      (let [t (* 0.5 t)
            a (mod (+ s (* 0.2 t)
                      (math/sin
                       (+ s (* 0.5 t)
                          (* 0.3 (math/sin (+ c (* tm/PHI (math/sin (* 0.2 t))))))))) 1.0)
            b (mod (+ (* c s) (math/sin (+ s c (math/sin (* 0.15 (+ s t))))))
                   1.0)
            line (gl/line2 (rp/sample-point-at polygon a)
                           (rp/sample-point-at polygon b))]
        ;; FIXME: clips outside if polygon is concave
        (qdg/draw line)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "Genuary 2026 - Day 2 - Twelve Principles of Animation"]
    [:p "Went with abstract math animation instead of something more closely
    following the principles."]]])

(sketch/definition hexagonal-intentions
  {:created-at "2026-01-02"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))

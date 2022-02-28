(ns shimmers.scratch.ellipse
  "Experiments in clipping part of an ellipse into a bounded-box."
  (:require
   [shimmers.math.equations :as eq]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn clockwise-intercept [center bounds]
  (let [midpoints (map (fn [[p q]] (tm/mix p q 0.5)) (g/edges bounds))]
    (apply min (map #(g/heading (tm/- % center)) midpoints))))

(comment (clockwise-intercept (gv/vec2 -5 11) (rect/rect 10)))

(defn ellipse-arc [center a b intercept dt]
  (for [t (range intercept (+ intercept eq/TAU) dt)]
    (tm/+ center (gv/vec2 (* a (Math/cos t))
                          (* b (Math/sin t))))))

(comment (ellipse-arc (gv/vec2 1 0) 10 10 0 0.1))


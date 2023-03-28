(ns shimmers.sketches.harsh-lines
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn lines []
  (for [y (range 0.2 0.8 0.1)]
    (gl/line2 (rv (dr/random 0.05 0.35) y)
              (rv (dr/random 0.65 0.95) (dr/gaussian y (/ y 45))))))

(defn verticle-line [line t height-sd angle-sd]
  (let [p (g/point-at line t)]
    (-> (gl/line2 [0 -1] [0 1])
        (g/rotate (dr/gaussian 0 angle-sd))
        (g/scale-size (dr/gaussian (* height 0.025) height-sd))
        (g/translate p))))

(defn shapes []
  (let [dx 0.003
        flip-row (dr/rand-nth [2 4 5])]
    (mapcat (fn [[i line]]
              (let [{[a b] :points} (g/scale-size line 1.03)]
                [(gl/line2 a b)
                 (csvg/group {:stroke-width 0.5}
                   (map (fn [x]
                          (let [t (dr/gaussian x (* x dx))
                                {[p q] :points}
                                (verticle-line (if (= i flip-row) (g/flip line) line)
                                               t
                                               (* x (* 0.2 (inc i)) (* height 0.01))
                                               (* 0.03 (* t (inc i))))]
                            (gl/line2 p q)))
                        (range 0 1 dx)))]))
            (map-indexed vector (lines)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
                  (shapes)))

(sketch/definition harsh-lines
  {:created-at "2021-04-07"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount (view-sketch/page-for scene :harsh-lines)
              "sketch-host"))

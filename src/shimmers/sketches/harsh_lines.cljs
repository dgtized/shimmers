(ns shimmers.sketches.harsh-lines
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:lines (for [y (range 0.2 0.8 0.1)]
            (gl/line2 (cq/rel-pos (dr/random 0.05 0.35) y)
                      (cq/rel-pos (dr/random 0.65 0.95) (dr/gaussian y (/ y 45)))))})

(defn verticle-line [line t height-sd angle-sd]
  (let [p (g/point-at line t)]
    (-> (gl/line2 [0 -1] [0 1])
        (g/rotate (dr/gaussian 0 angle-sd))
        (g/scale-size (dr/gaussian (cq/rel-h 0.025) height-sd))
        (g/translate p))))

(defn draw [{:keys [lines]}]
  (doseq [[i line] (map-indexed vector lines)]
    (let [dx 0.003
          flip-row (dr/rand-nth [2 4 5])
          {[a b] :points} (g/scale-size line 1.03)]
      (q/stroke-weight 1.0)
      (q/line a b)
      (q/stroke-weight 0.3)
      (doseq [x (range 0 1 dx)]
        (let [t (dr/gaussian x (* x dx))
              {[p q] :points}
              (verticle-line (if (= i flip-row) (g/flip line) line)
                             t
                             (* x (* 0.2 (inc i)) (cq/rel-h 0.01))
                             (* 0.03 (* t (inc i))))]
          (q/line p q))))))

;; TODO: convert to SVG
(sketch/defquil harsh-lines
  :created-at "2021-04-07"
  :tags #{:static :deterministic}
  :size [900 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode])

(ns shimmers.sketches.chaos-cuts
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]))

;; alternative idea for future: use multiple layers, ie have a pendulum or shape
;; with edges cut the top polygon, cut out a pane, only to reveal another
;; polygon being cut by new lines/pendulums

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.8)]
    {:bounds bounds
     :shapes []
     :t 0}))

(defn cut [line shape]
  (->> (lines/cut-polygon shape line)
       (mapcat (fn [poly]
                 (->> (poly-detect/inset-polygon poly 4)
                      #_poly-detect/split-self-intersection
                      poly-detect/self-intersection-polygons
                      (filter (fn [inset] (> (Math/abs (g/area inset)) 100))))))))

(defn cline [p0 r0 p1 r1 t0 t1]
  (let [a (gc/circle (apply cq/rel-vec p0) (cq/rel-h r0))
        b (gc/circle (apply cq/rel-vec p1) (cq/rel-h r1))]
    (gl/line2 (g/point-at a t0)
              (g/point-at b t1))))

(defn gen-lines [t]
  [(cline [0.5 -3] 1.2
          [0.5 4] 1.1
          (+ 1 (* 0.01 t)) (+ 2 (* 0.02 t)))
   (cline [0.5 -3] 1.05
          [0.5 4] 1.1
          (* 0.025 t) (* 0.015 t))
   (cline [-3 0.5] 0.8
          [2.5 0.5] 1.1
          (+ 1 (* 0.03 t)) (+ 2 (* 0.04 t)))
   (cline [-2.5 (+ 0.5 (* 0.3 (Math/sin (* 0.01 t))))] 1.1
          [2.25 0.5] 0.8
          (+ 2 (* 0.03 t)) (+ 3 (* 0.035 t)))])

(defn update-state [{:keys [t bounds] :as state}]
  (let [lines (gen-lines t)]
    (-> state
        (update :t + (* 0.1 (Math/abs (dr/gaussian 0.08 0.1))))
        (assoc :shapes
               (reduce (fn [polygons line]
                         (mapcat (fn [poly] (cut line poly)) polygons))
                       [bounds]
                       lines)))))

(defn draw [{:keys [bounds shapes]}]
  (q/background 1.0)
  (q/fill 1.0)
  (doseq [polygon shapes]
    (if (collide/bounded? bounds polygon)
      (q/stroke 0)
      (q/stroke 0 0.5 0.5))
    (if (poly-detect/clockwise-polygon? (g/vertices polygon))
      (q/fill 1.0)
      (q/fill 0.0))
    (cq/draw-polygon polygon)))

(sketch/defquil chaos-cuts
  :created-at "2022-12-04"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

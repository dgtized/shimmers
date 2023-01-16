(ns shimmers.sketches.brush-strokes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.line :as gl]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.geometry :as geometry]))

(defn generate-brush [line]
  (let [len (tm/mag line)]
    {:point (g/centroid line)
     :line line
     :facing (gv/vec2 1 0)
     :vel (gv/vec2 1 0)
     :angle-vel 0.0
     :bristles
     (vec
      (for [[a b] (partition 2 (dr/gaussian-range 0.12 0.03 true))]
        (let [c (+ a (* 0.5 (- b a)))]
          (triangle/inscribed-equilateral
           (gc/circle (g/point-at line c) (* 1.5 len (- b a)))
           (dr/random eq/TAU)))))}))

(defn translate-brush [brush p]
  (-> brush
      (update :point g/translate p)
      (update :line g/translate p)
      (update :bristles (partial mapv (fn [b] (g/translate b p))))))

(defn rotate-brush [{:keys [point] :as brush} t]
  (-> brush
      (update :facing g/rotate t)
      (update :line geometry/rotate-around point t)
      (update :bristles (partial mapv (fn [b] (geometry/rotate-around b point t))))))

(defn rotate-bristles [brush t]
  (-> brush
      (update :bristles (partial mapv (fn [b] (geometry/rotate-around-centroid b t))))))

;; see also https://gamedev.stackexchange.com/questions/1885/target-tracking-when-to-accelerate-and-decelerate-a-rotating-turret
(defn follow [{:keys [point facing vel angle-vel] :as brush} target dt]
  (let [dir (tm/- target point)
        dv (tm/* dir (/ (* 200 dt) (tm/mag-squared dir)))
        vel' (tm/* (tm/+ vel dv) 0.97)
        pos' (tm/+ point (tm/* vel dt))
        delta-angle (let [delta (- (g/heading dir) (g/heading facing))]
                      (cond (< delta (- Math/PI)) (+ delta eq/TAU)
                            (> delta Math/PI) (- delta eq/TAU)
                            :else delta))
        c0 0.0001
        c1 (* 2 (Math/sqrt c0))
        angle-acc (* dt (- (* c0 delta-angle) (* c1 angle-vel)))
        angle-vel' (+ angle-vel angle-acc)]
    (-> brush
        (translate-brush (tm/- pos' point))
        (rotate-brush angle-vel)
        (rotate-bristles (* angle-vel (/ 1 3)))
        (assoc :vel vel'
               :angle-vel angle-vel'))))

(defn generate-path []
  (vec (reverse (into [(cq/rel-vec 0.05 0.5)
                       (cq/rel-vec 0.85 0.5)]
                      (repeatedly 14 #(cq/rel-vec (dr/random 0.15 0.85)
                                                  (dr/random 0.15 0.85)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [path (generate-path)
        next-pt (peek path)
        h (cq/rel-vec 0.0 0.05)]
    {:t 0
     :path (pop path)
     :brush (generate-brush
             (gl/line2 (tm/- next-pt h) (tm/+ next-pt h)))}))

(defn update-state [{:keys [brush path] :as state}]
  (let [dt 0.25
        next-pt (peek path)]
    (if next-pt
      (-> (if (< (g/dist next-pt (:point brush)) (cq/rel-h 0.05))
            (update state :path pop)
            state)
          (update :t + dt)
          (update :brush follow next-pt dt))
      state)))

(defn draw [{:keys [brush path]}]
  (when (seq path)
    (q/no-stroke)
    (q/fill 0.0 0.05)
    (doseq [hair (:bristles brush)]
      (cq/draw-polygon hair))))

(sketch/defquil brush-strokes
  :created-at "2023-01-15"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.triangle-flow
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.core :as sm]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.math.core :as tm]))

(defn noise-at-p [bounds p t]
  (let [[rx ry] (tm/* (g/map-point bounds p) 2)
        x (sm/reflect-into rx 2)
        y (sm/reflect-into ry 2)]
    (mod (* 2 (q/noise (* 0.5 x) (* 0.5 y) t)) 1)))

(defn make-particle [pos velocity]
  {:pos pos
   :last-pos (tm/- pos velocity)
   :dt (tm/random eq/TAU)})

(defn update-particle [bounds t {:keys [pos last-pos dt] :as particle}]
  (let [velocity (tm/- pos last-pos)
        n (noise-at-p bounds pos (+ t dt))
        accel (v/polar 0.2 (* eq/TAU n))
        velocity' (tm/normalize (tm/+ velocity accel) 0.8)
        pos' (tm/+ pos velocity')]
    (if (g/contains-point? bounds pos')
      (assoc particle
             :pos pos'
             :last-pos pos)
      (let [wrapped-pos (v/wrap2d pos' (g/width bounds) (g/height bounds))]
        (assoc particle
               :pos wrapped-pos
               :last-pos (tm/- wrapped-pos velocity'))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :t 0
     :particles (repeatedly 12 #(make-particle (g/random-point-inside (g/scale-size bounds 0.8))
                                               (v/polar 3 (* eq/TAU (tm/random)))))}))

(defn update-state [{:keys [bounds t] :as state}]
  (-> state
      (update :t + 0.005)
      (update :particles (partial map (partial update-particle bounds t)))))

(defn brush-at [rotate scale p]
  (-> (gt/triangle2 [-1 -1] [1 -1] [0 1])
      (g/rotate rotate)
      (g/scale-size scale)
      (g/translate p)))

(defn draw [{:keys [bounds t particles]}]
  #_(q/background 1.0 0.05)
  #_(doseq [i (tm/norm-range 45)
            j (tm/norm-range 45)
            :let [p (g/unmap-point bounds (gv/vec2 i j))]]
      (q/line p (tm/+ p (v/polar 10 (* eq/TAU (noise-at-p bounds p t))))))

  (doseq [{:keys [pos last-pos dt]} particles
          :let [t (+ t dt)
                vis (tm/smoothstep* 0.3 0.75 (q/noise 0.1 t))]]
    #_(q/line last-pos pos)
    (q/stroke 0 (* vis 0.06))
    (q/fill 0 (* vis 0.03))
    (cq/draw-polygon
     (brush-at
      (* 4 (tm/smoothstep* 0.2 0.8 (mod t 1)) t)
      10
      (tm/mix pos last-pos 0.5)))))

(sketch/defquil triangle-flow
  :created-at "2022-04-13"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

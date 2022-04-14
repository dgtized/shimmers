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
        y (sm/reflect-into ry 2)
        s 0.1]
    (q/noise (* s x) (* s y) (* s t))))

(defn make-particle [pos velocity]
  {:pos pos
   :last-pos (tm/- pos velocity)
   :dt (tm/random eq/TAU)})

(defn update-particle [bounds t {:keys [pos last-pos dt] :as particle}]
  (let [n (noise-at-p bounds pos t)
        accel (v/polar 0.1 (* 3 eq/TAU n))
        velocity (tm/- pos last-pos)
        velocity' (tm/* (tm/+ velocity accel) 0.9)
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
                vis (tm/smoothstep* 0.3 0.75 (q/noise dt (* t 0.25)))
                color (Math/abs (Math/sin t))
                grey (tm/smoothstep* 0.4 0.6 vis)]]
    #_(q/line last-pos pos)
    (if (> color 0.75)
      (do
        (q/stroke grey (* vis 0.2))
        (q/fill (q/noise (* t 0.05) dt)
                0.5 0.5 (* 0.1 vis)))
      (do
        (q/stroke grey (* vis 0.1))
        (q/fill grey (* vis 0.04))))
    (cq/draw-polygon
     (brush-at
      (* 3 (tm/smoothstep* 0.2 0.8 (eq/unit-cos t)) t)
      (+ 2 (* 30 (q/noise dt (* t 0.1))))
      (tm/mix pos last-pos 0.5)))))

(sketch/defquil triangle-flow
  :created-at "2022-04-13"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

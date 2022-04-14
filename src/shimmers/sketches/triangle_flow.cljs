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
    (q/noise x y t)))

(defn make-particle [pos velocity]
  {:pos pos
   :last-pos (tm/- pos velocity)})

(defn update-particle [bounds t {:keys [pos last-pos] :as particle}]
  (let [velocity (tm/- pos last-pos)
        velocity' (tm/normalize (tm/+ velocity (v/polar 0.05 (* eq/TAU (noise-at-p bounds pos t)))) 0.75)
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
     :particles (repeatedly 24 #(make-particle (g/random-point-inside (g/scale-size bounds 0.8))
                                               (v/polar 3 (* eq/TAU (tm/random)))))}))

(defn update-state [{:keys [bounds t] :as state}]
  (-> state
      (update :t + 0.005)
      (update :particles (partial map (partial update-particle bounds t)))))

(defn brush-at [p t]
  (-> (gt/triangle2 [-1 -1] [1 -1] [0 1])
      (g/rotate t)
      (g/scale-size 8)
      (g/translate p)))

(defn draw [{:keys [bounds t particles]}]
  (q/background 1.0 0.2)
  #_(doseq [i (tm/norm-range 45)
            j (tm/norm-range 45)
            :let [p (g/unmap-point bounds (gv/vec2 i j))]]
      (q/line p (tm/+ p (v/polar 10 (* eq/TAU (noise-at-p bounds p t))))))

  (doseq [{:keys [pos last-pos]} particles]
    #_(q/line last-pos pos)
    (q/fill 0 0.5)
    (cq/draw-polygon (brush-at (tm/mix pos last-pos 0.5) t))))

(sketch/defquil triangle-flow
  :created-at "2022-04-13"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

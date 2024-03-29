(ns shimmers.sketches.triangle-flow
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn noise-at-p [bounds p t]
  (let [[rx ry] (tm/* (g/map-point bounds p) 2)
        x (sm/reflect-into rx 2)
        y (sm/reflect-into ry 2)
        s 0.5]
    (q/noise (* s x) (* s y) (* s t))))

(defn make-particle [pos velocity]
  {:pos pos
   :last-pos (tm/- pos velocity)
   :color (dr/random 0.66)
   :dt (dr/random-tau)})

(defn update-particle [bounds t {:keys [pos last-pos dt] :as particle}]
  (let [n (noise-at-p bounds pos t)
        rf (+ (* (tm/smoothstep* 0.2 0.8 (eq/unit-sin (* 0.5 (+ t dt)))) 0.15) 0.10)
        accel (tm/+ (v/polar rf (* 2 eq/TAU n))
                    (v/polar 0.05 (dr/random (* 2 eq/TAU))))
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
  (q/noise-seed (dr/seed))
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :t 0
     :particles (map (fn [p] (make-particle p (v/polar 3 (dr/random-tau))))
                     (rp/random-points (g/scale-size bounds 0.8) 12))}))

(defn update-state [{:keys [bounds t] :as state}]
  (-> state
      (update :t + 0.005)
      (update :particles (partial map (partial update-particle bounds t)))))

(defn brush-at [rotate scale p]
  (-> (gt/triangle2 [-1 -1] [1 -1] [0 1])
      (g/rotate rotate)
      (g/scale-size scale)
      (g/translate p)))

(defn draw-field [{:keys [bounds t]}]
  (q/background 1.0 0.05)
  (q/stroke 0.0 0.1)
  (doseq [i (tm/norm-range 45)
          j (tm/norm-range 45)
          :let [p (g/unmap-point bounds (gv/vec2 i j))]]
    (q/line p (v/+polar p 10 (* eq/TAU (noise-at-p bounds p t))))))

(defn draw-flow [{:keys [t particles]}]
  (doseq [{:keys [pos last-pos dt color]} particles
          :let [t (+ t dt)
                vis (tm/smoothstep* 0.3 0.75 (q/noise dt (* t 0.25)))
                grey (tm/smoothstep* 0.4 0.6 vis)]]
    (if (> color (eq/unit-sin (* 0.2 t)))
      (let [[x y] pos
            hue (mod (* tm/PHI (q/noise (* x 0.001) (* y 0.001) (* t 0.05))) 1)]
        (q/stroke grey (* vis 0.2))
        (q/fill hue 0.5 0.5 (* 0.1 vis)))
      (do
        (q/stroke grey (* vis 0.2))
        (q/fill grey (* vis 0.08))))
    (cq/draw-polygon
     (brush-at
      (* 3 (tm/smoothstep* 0.2 0.8 (eq/unit-cos t)) t)
      (+ 2 (* 38 (q/noise dt (* t 0.1))))
      (tm/mix pos last-pos 0.5)))))

(def modes {:flow draw-flow
            :field draw-field})

(defonce ui-state (ctrl/state {:mode :flow}))

(defn draw [state]
  ((get modes (:mode @ui-state)) state))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ctrl/change-mode ui-state (keys modes)]])

(sketch/definition triangle-flow
  {:created-at "2022-04-13"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))

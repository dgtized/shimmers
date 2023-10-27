(ns shimmers.sketches.woven
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn gen-threads [n pass]
  (for [t (range n)]
    (let [o (+ (/ (float (inc t)) (inc n)) (dr/gaussian 0.0 (/ 0.33 (inc n))))]
      (case (mod pass 4)
        0 [(cq/rel-vec -0.1 o) 0.0 v/right]
        1 [(cq/rel-vec o -0.1) 0.0 v/up]
        2 [(cq/rel-vec 1.1 o) 0.0 v/left]
        3 [(cq/rel-vec o 1.1) 0.0 v/down]))))

(defn gen-n []
  (dr/weighted {5 1
                7 2
                11 3
                13 2
                17 1}))

(defn choose-rate []
  (dr/random 0.0002 0.01))

(def black&white
  [0.0 0.0 0.0])

(defn random-color [mono]
  (dr/weighted
   {[0.6 0.8 0.5] (if mono 0 1)
    [0.0 0.8 0.5] (if mono 0 1)
    black&white 4}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/noise-seed (dr/seed))
  (let [pass 0
        n (gen-n)
        mono (dr/chance 0.6)]
    {:seed (cq/rel-vec (dr/random-vertex))
     :n n
     :pass pass
     :rate (choose-rate)
     :triangles (gen-threads n pass)
     :screen (cq/screen-rect)
     :color (random-color mono)
     :mono mono
     :t (q/millis)}))

(defn outside? [screen [pos _ dir]]
  (and (not (g/contains-point? screen pos))
       (case dir
         v/right (> (:x pos) (cq/rel-w 1.1))
         v/left (< (:x pos) (cq/rel-w -0.1))
         v/up (> (:y pos) (cq/rel-h 1.1))
         v/down (< (:y pos) (cq/rel-h -0.1)))))

(defn update-pos [rate t dt [pos rot dir]]
  [(tm/+ pos (tm/* dir (* 0.075 dt)))
   (+ rot (* (* 0.005 (Math/sin (* rate t))) dt))
   dir])

(defn update-state [{:keys [mono triangles rate screen pass t] :as state}]
  (let [dt (- (q/millis) t)]
    (-> (if (every? (partial outside? screen) triangles)
          (let [n (gen-n)]
            (-> state
                (update :pass inc)
                (assoc :n n
                       :rate (choose-rate)
                       :color (random-color mono)
                       :triangles (gen-threads n (inc pass)))))
          state)
        (update :t + dt)
        (update :seed tm/+ (tm/* (gv/vec2 0.00001 0.00001) t))
        (update :triangles (partial map (partial update-pos rate t dt))))))

(defn draw [{:keys [seed pass color triangles n]}]
  (if (< pass 4)
    (let [r (cq/rel-h (/ 0.15 (inc n)))]
      (doseq [[pos rot _] triangles]
        (let [n (apply q/noise (tm/* (tm/+ seed pos) 0.006))
              n2 (apply q/noise (tm/+ (gv/vec2 50 50) (tm/* (tm/+ seed pos) 0.005)))]
          (q/fill 0.0 (+ 0.0001 (* n 0.0225)))
          (apply q/stroke (conj color (+ 0.001 (* n 0.225))))
          (let [triangle (triangle/inscribed-equilateral pos (* (+ 0.25 (* 2.25 n2)) r) rot)]
            (cq/draw-triangle (g/vertices triangle))))))
    (q/no-loop)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition woven
  {:created-at "2023-10-25"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

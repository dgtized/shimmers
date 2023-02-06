(ns shimmers.sketches.ballistics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defrecord Turret [pos dir])
(defrecord Shell [pos vel mass])
(defrecord Missile [pos vel mass fuel])

(defn generate-turret [ground-pos]
  (let [p (tm/+ ground-pos (cq/rel-vec 0.0 -0.01))]
    (->Turret p (tm/normalize (tm/- (cq/rel-vec 0.5 0.0) p)))))

(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  (let [ground (->> (concat [0.0] (dr/gaussian-range 0.03 0.01) [1.0])
                    (mapv (fn [x] (cq/rel-vec x (- 1.0 (* 0.4 (q/noise (* 4 x) 0.5))))))
                    gl/linestrip2)]
    {:turrets [(generate-turret (g/point-at ground (dr/random 0.05 0.45)))
               (generate-turret (g/point-at ground (dr/random 0.55 0.95)))]
     :projectiles []
     :ground ground}))

(defn maybe-add-projectile [{:keys [projectiles turrets] :as state}]
  (if (and (< (count projectiles) 5) (dr/chance 0.05))
    (let [{:keys [pos dir]} (dr/rand-nth turrets)]
      (update state :projectiles conj
              (->Shell (tm/+ pos (tm/* dir 4.0))
                       (tm/* dir (dr/random 6 11))
                       3.0)))
    state))

(defn update-projectile [ground dt {:keys [pos vel] :as projectile}]
  (if (> (:y pos) (:y (g/point-at ground (/ (:x pos) (q/width)))))
    nil
    (-> projectile
        (update :pos tm/+ vel)
        (update :vel (fn [v] (tm/* (tm/+ v (gv/vec2 0 (* dt 9.8))) 0.99))))))

(defn update-state [{:keys [ground] :as state}]
  (let [dt 0.01]
    (-> state
        maybe-add-projectile
        (update :projectiles (partial keep (partial update-projectile ground dt))))))

(defn turret-shapes [{:keys [pos dir]}]
  (let [s (cq/rel-h 0.015)]
    [(-> (rect/rect 0 0 (* tm/PHI s) s)
         g/center
         (g/translate pos))
     (gl/line2 pos (tm/+ pos (tm/* dir (* 1.1 s))))]))

(defn draw [{:keys [ground turrets projectiles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)

  (let [verts (g/vertices ground)]
    (cq/draw-curve-path
     (concat [(gv/vec2 (- 0.0 (* (q/width) 0.05)) (:y (first verts)))]
             verts
             [(gv/vec2 (* (q/width) 1.05) (:y (last verts)))])))

  (doseq [turret turrets]
    (doseq [s (turret-shapes turret)]
      (qdg/draw s)))

  (doseq [{:keys [pos mass]} projectiles]
    (cq/circle pos mass)))

(sketch/defquil ballistics
  :created-at "2023-02-05"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

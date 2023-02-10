(ns shimmers.sketches.ballistics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.control :as control]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn start-dist [mass]
  (* 2.0 mass))

(defn contact-dist [mass]
  (* tm/PHI mass))

(defn explode-dist [mass]
  (* 5.0 mass))

(defrecord Turret [health pos dir angle-target angle-vel target])
(defrecord Shell [pos vel mass])
(defrecord Missile [pos vel mass fuel])

(defn generate-turret [ground-pos]
  (let [p (tm/+ ground-pos (cq/rel-vec 0.0 -0.01))
        dir (tm/normalize (tm/- (cq/rel-vec 0.5 0.0) p))]
    (->Turret 1.0 p dir (g/heading dir) 0.0 nil)))

(defn make-turrets [ground n]
  (let [margin (* 0.08 (/ 1.0 n))]
    (for [i (range n)
          :let [p (dr/random (+ (/ (float i) n) margin)
                             (- (/ (float (inc i)) n) margin))]]
      (generate-turret (g/point-at ground p)))))

(defn initial-state []
  (let [ground (->> (concat [0.0] (dr/gaussian-range 0.03 0.01) [1.0])
                    (mapv (fn [x] (cq/rel-vec x (- 1.0 (* 0.4 (q/noise (* 4 x) 0.5))))))
                    gl/linestrip2)]
    {:turrets (make-turrets ground (dr/random-int 2 7))
     :projectiles []
     :ground ground}))

(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  (initial-state))

(defn maybe-add-projectile [{:keys [projectiles turrets] :as state}]
  (if (and (< (count projectiles) 12) (seq turrets) (dr/chance 0.03))
    (let [{:keys [pos dir]} (dr/rand-nth turrets)
          muzzle-velocity (tm/* dir (dr/random-int 4 13))
          mass (dr/weighted {3.0 2.0
                             3.5 1.5
                             4.0 1.0})]
      (update state :projectiles conj
              (->Shell (tm/+ pos (tm/* dir (start-dist mass)))
                       muzzle-velocity
                       mass)))
    state))

(defn update-projectile [ground turrets dt {:keys [pos vel explode mass] :as projectile}]
  (let [ground-point (g/point-at ground (/ (:x pos) (q/width)))]
    (cond (and explode (<= explode 0))
          nil
          (and explode (> explode 0))
          (update projectile :explode dec)
          (or (> (:y (tm/+ pos vel)) (:y ground-point))
              (some (fn [{turret :pos}]
                      (< (g/dist turret pos) (contact-dist mass)))
                    turrets))
          (-> projectile
              (assoc :vel (gv/vec2)
                     :explode (dr/random-int 4 8)))
          :else
          (-> projectile
              (update :pos tm/+ vel)
              (update :vel (fn [v] (tm/* (tm/+ v (gv/vec2 0 (* dt 9.8))) 0.99)))))))

(defn pick-target [{:keys [pos]} turrets]
  (some->> turrets
           (remove (fn [other] (< (g/dist pos (:pos other)) 1.0)))
           seq
           dr/rand-nth))

(defn no-target? [target turrets]
  (if (nil? target)
    true
    (not-any? (fn [{:keys [pos]}]
                (< (g/dist (:pos target) pos) 1.0))
              turrets)))

(defn firing-range [margin {:keys [pos target]}]
  (when target
    (let [target-angle (tm/clamp (g/heading (tm/- (:pos target) pos))
                                 (* 0.5 eq/TAU) eq/TAU)]
      (if (> target-angle (* eq/TAU 0.75))
        [(+ (* eq/TAU 0.75) margin) target-angle]
        [target-angle (- (* eq/TAU 0.75) margin)]))
    [(* 0.5 eq/TAU) eq/TAU]))

(defn exploding-damage [turret-pos exploding]
  (reduce (fn [tot {:keys [pos mass]}]
            (let [dist (g/dist turret-pos pos)]
              (if (< dist (explode-dist mass))
                (+ tot (/ (* 3 mass) (* dist dist)))
                tot)))
          0.0
          exploding))

(defn rotate-turret [{:keys [angle-target angle-vel] :as turret} angle-dir dt]
  (let [angle-acc (control/angular-acceleration angle-dir angle-target
                                                0.6 angle-vel)]
    (-> turret
        (assoc :angle-vel (+ angle-vel angle-acc))
        (update :dir g/rotate (* dt angle-vel)))))

(defn update-turret
  [exploding turrets dt]
  (fn [state {:keys [pos dir angle-target target health] :as turret}]
    (if (< health 0.0)
      state
      (let [angle-dir (g/heading dir)
            damage (exploding-damage pos exploding)
            rotating? (> (sm/radial-distance angle-dir angle-target) 0.01)
            new-target? (no-target? target turrets)
            turret'
            (cond-> turret
              (> damage 0)
              (update :health - damage)
              rotating?
              (rotate-turret angle-dir dt)
              new-target?
              (assoc :target (pick-target turret turrets)))]
        (update state :turrets conj
                (if (or new-target? (dr/chance 0.05))
                  (let [angle (apply dr/random (firing-range 0.02 turret'))]
                    (assoc turret' :angle-target angle))
                  turret'))))))

(defn update-state [{:keys [ground projectiles turrets] :as state}]
  (let [dt 0.01
        exploding (filter (fn [{:keys [explode]}] (> explode 0)) projectiles)]
    (if (and (empty? projectiles) (<= (count turrets) 1))
      (initial-state)
      (let [state' (-> state
                       maybe-add-projectile
                       (update :projectiles (partial keep (partial update-projectile ground turrets dt))))]
        (reduce (update-turret exploding turrets dt)
                (assoc state' :turrets [])
                turrets)))))

(defn turret-shapes [{:keys [pos dir health]}]
  (let [s (cq/rel-h 0.015)
        health-w (* 1.33 s)]
    [(-> (rect/rect 0 0 (* tm/PHI s) s)
         g/center
         (g/translate pos))
     (let [barrel-width (* 0.33 s)
           length (* 1.25 s (tm/mag dir))]
       (-> (rect/rect 0 (* -0.5 barrel-width) length barrel-width)
           (g/rotate (g/heading dir))
           (g/translate pos)
           (assoc :fill 1.0)))
     (-> (rect/rect (* -0.5 health-w) (* 0.8 s) health-w (* 0.4 s))
         (g/translate pos)
         (assoc :fill 1.0))
     (-> (rect/rect (* -0.5 health-w) (* 0.8 s) (* health-w health) (* 0.4 s))
         (g/translate pos)
         (assoc :fill 0.0))]))

(defn draw [{:keys [ground turrets projectiles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)

  (q/no-fill)
  (let [verts (g/vertices ground)]
    (cq/draw-curve-path
     (concat [(gv/vec2 (- 0.0 (* (q/width) 0.05)) (:y (first verts)))]
             verts
             [(gv/vec2 (* (q/width) 1.05) (:y (last verts)))])))

  (doseq [turret turrets]
    (doseq [{:keys [fill] :as s} (turret-shapes turret)]
      (if fill
        (q/fill fill)
        (q/no-fill))
      (qdg/draw s)))

  (q/fill 0.25)
  (doseq [{:keys [pos mass explode]} projectiles]
    (cq/circle pos
               (if (> explode 0)
                 (dr/random (* 0.5 mass) (* 3.0 mass))
                 mass))))

(sketch/defquil ballistics
  :created-at "2023-02-05"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

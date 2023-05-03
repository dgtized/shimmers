(ns shimmers.sketches.ballistics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.control :as control]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn start-dist [mass]
  (* 2.0 mass))

(defn contact-dist [mass]
  (* tm/PHI mass))

(defn explode-dist [mass]
  (* 5.0 mass))

(defrecord Turret [health pos angle angle-target angle-vel target firing-cycle])
(defrecord Shell [pos vel mass flight-time])
(defrecord Cluster [pos vel mass flight-time])
(defrecord Missile [pos vel mass fuel])

(defn generate-turret [ground-pos]
  (let [p (tm/+ ground-pos (cq/rel-vec 0.0 -0.01))
        angle (g/heading (tm/- (cq/rel-vec 0.5 0.0) p))]
    (->Turret 1.0 p angle angle 0.0 nil 0.1)))

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

(defn initial-velocity [angle [x y]]
  (Math/sqrt
   (abs
    (/ (* (eq/sqr x) 9.8)
       (- (* x (Math/sin (* 2 angle)))
          (* 2 y (eq/sqr (Math/cos angle))))))))

;; need to account for vertical height of target?
(defn time-of-flight [angle v0 x]
  (abs (/ x (* v0 (Math/cos angle)))))

(defn fire-projectile [state {:keys [pos angle target]}]
  (let [dir (v/polar 1.0 angle)
        [x y] (tm/abs (tm/- (:pos target) pos))
        ;; v0 is ignoring drag
        v0 (initial-velocity angle [x y])
        v0' (* v0 (dr/gaussian 1.0 0.12))
        muzzle-velocity (tm/* dir v0')
        mass (dr/weighted {3.0 2.0
                           3.5 1.5
                           4.0 1.0})
        initial-pos (tm/+ pos (tm/* dir (start-dist mass)))
        flight-time (time-of-flight angle v0' x)
        projectile (if (dr/chance 0.8)
                     (->Shell initial-pos muzzle-velocity mass
                              flight-time)
                     (->Cluster initial-pos muzzle-velocity mass
                                (* (dr/gaussian 0.75 0.08) flight-time)))]
    (update state :projectiles conj projectile)))

;; TODO: check projectile/projectile collisions?
(defn update-projectile [ground turrets dt]
  (fn [state {:keys [pos vel explode mass flight-time] :as projectile}]
    (let [ground-point (g/point-at ground (/ (:x pos) (q/width)))
          pos' (tm/+ pos (tm/* vel dt))
          proj' (cond (and explode (<= explode 0))
                      nil
                      (and explode (> explode 0))
                      (update projectile :explode dec)
                      (or (> (:y pos') (:y ground-point))
                          (some (fn [{turret :pos}]
                                  (< (g/dist turret pos) (contact-dist mass)))
                                turrets))
                      (-> projectile
                          (assoc :vel (gv/vec2)
                                 :explode (dr/random-int 4 8)))
                      (and (instance? Cluster projectile)
                           (< flight-time 0.0))
                      (-> projectile
                          (assoc :vel (gv/vec2)
                                 :burst (dr/random-int 3 12)
                                 :explode (dr/random-int 2 5)))
                      :else
                      (let [drag (- 1.0 (eq/sqr (* 1.1 dt)))
                            gravity (gv/vec2 0 (* dt 9.8))]
                        (-> projectile
                            (assoc :pos pos')
                            (update :vel (fn [v] (tm/* (tm/+ v gravity) drag))))))
          burst (get proj' :burst 0)]
      (cond-> state
        proj'
        (update :projectiles conj (update (dissoc proj' :burst) :flight-time - dt))
        (> burst 0)
        (update :projectiles into
                (repeatedly burst
                            (fn []
                              (->Shell pos
                                       (tm/+ vel (tm/* (gv/vec2 (dr/gaussian 0.0 3.0) 0.0)))
                                       (* 2.0 (/ mass burst))
                                       1.0))))))))

(defn pick-target [{:keys [pos]} turrets]
  (some->> turrets
           (remove (fn [other] (< (g/dist pos (:pos other)) 1.0)))
           seq
           dr/rand-nth))

(defn no-target? [target turrets]
  (if (nil? target)
    true
    (not-any? (fn [{:keys [pos health]}]
                (and (> health 0.0)
                     (< (g/dist (:pos target) pos) 1.0)))
              turrets)))

;; https://en.wikipedia.org/wiki/Projectile_motion
(defn firing-range [margin {:keys [pos target]}]
  (if target
    (let [zenith (* eq/TAU 0.75)
          target-angle
          (as-> (g/heading (tm/- (:pos target) pos)) heading
            ;; zenith to quarter tau is right, not left facing
            (if (< heading (* 0.25 eq/TAU)) (+ heading eq/TAU) heading)
            (tm/clamp heading (* 0.5 eq/TAU) eq/TAU))
          ;; this is the lowest energy angle to fire at
          halfway-angle (/ (+ zenith target-angle) 2.0)]
      (if (dr/chance 0.66)
        (let [error (dr/random margin (* 3 margin))]
          [(- halfway-angle error) (+ halfway-angle error)])
        (if (>= target-angle zenith)
          [(+ zenith (* 2 margin)) (- target-angle margin)]
          [(+ target-angle margin) (- zenith (* 2 margin))])))
    [(* 0.5 eq/TAU) eq/TAU]))

(defn exploding-damage [turret-pos exploding]
  (reduce (fn [tot {:keys [pos mass]}]
            (let [dist (g/dist turret-pos pos)]
              (if (< dist (explode-dist mass))
                (+ tot (/ (* 3 mass) (* dist dist)))
                tot)))
          0.0
          exploding))

(defn rotate-turret [{:keys [angle angle-target angle-vel] :as turret} dt]
  (let [angle-acc (control/angular-acceleration angle angle-target
                                                (/ 0.33 dt) angle-vel)]
    (-> turret
        (assoc :angle-vel (+ angle-vel (* angle-acc dt)))
        (update :angle + (* angle-vel dt)))))

(defn adjust-angle [turret]
  (let [angle (apply dr/random (firing-range 0.02 turret))]
    (assoc turret :angle-target angle)))

(defn update-turret
  [exploding turrets dt]
  (fn [state {:keys [pos angle angle-target target health firing-cycle] :as turret}]
    (let [alive? (> health 0.0)
          dead? (< health 0.0)
          damage (exploding-damage pos exploding)
          rotating? (and alive? (> (sm/radial-distance angle angle-target) 0.01))
          new-target? (and alive? (no-target? target turrets))
          can-fire? (and alive?
                         (<= firing-cycle 0.0)
                         (not (or rotating? new-target?))
                         (< (count (:projectiles state)) 16))
          firing? (and can-fire? (dr/chance 0.1))
          status (->> [(when alive? :alive)
                       (when rotating? :rotating)
                       (when new-target? :new-target)
                       (when can-fire? :can-fire)]
                      (keep identity)
                      set)
          turret'
          (cond-> turret
            (> damage 0)
            (update :health - damage)
            dead?
            (update :health - (dr/random 0.01 0.1))
            (> firing-cycle 0)
            (update :firing-cycle - dt)
            rotating?
            (rotate-turret dt)
            new-target?
            (assoc :target (pick-target turret turrets))
            (or new-target? (dr/chance 0.005))
            (adjust-angle)
            firing?
            (assoc :firing-cycle (dr/random 0.8 3.5)))]
      (cond-> state
        (> health -3.0) (update :turrets conj
                                (assoc turret' :status status))
        firing? (fire-projectile turret)))))

(defn debug! [{:keys [turrets projectiles] :as state}]
  (reset! defo {})
  (swap! defo assoc
         :projectiles (count projectiles)
         :turrets
         (for [t turrets]
           (->>
            (update t :target
                    (fn [{:keys [pos] :as target}]
                      (when target
                        [pos (g/heading (tm/- pos (:pos t)))])))
            (into {}))))
  state)

(defn update-state [{:keys [ground projectiles turrets] :as state}]
  (let [dt (/ 1 20)
        exploding (filter (fn [{:keys [explode]}] (> explode 0)) projectiles)]
    (if (and (empty? projectiles) (<= (count turrets) 1))
      (initial-state)
      (as-> state state
        (reduce (update-projectile ground turrets dt)
                (assoc state :projectiles [])
                projectiles)
        (reduce (update-turret exploding turrets dt)
                (assoc state :turrets [])
                turrets)
        (debug! state)))))

(defn turret-shapes [{:keys [pos angle health]}]
  (let [s (cq/rel-h 0.015)
        health-w (* 1.33 s)]
    (into
     [(-> (rect/rect 0 0 (* tm/PHI s) s)
          g/center
          (g/translate pos))
      (let [barrel-width (* 0.33 s)
            length (* 1.25 s)]
        (-> (rect/rect 0 (* -0.5 barrel-width) length barrel-width)
            (g/rotate angle)
            (g/translate pos)
            (assoc :fill 1.0)))
      (-> (rect/rect (* -0.5 health-w) (* 0.8 s) health-w (* 0.4 s))
          (g/translate pos)
          (assoc :fill 1.0))
      (-> (rect/rect (* -0.5 health-w) (* 0.8 s) (* health-w (tm/clamp01 health)) (* 0.4 s))
          (g/translate pos)
          (assoc :fill 0.0))]
     (if (< health 0.0)
       (repeatedly (dr/random-int 5)
                   #(let [r (tm/clamp (dr/gaussian 5.0 3.0) 2.0 12.0)
                          dist (tm/clamp (dr/gaussian (- (* 2.0 s) r) s) 0 (* 8 s))]
                      (assoc
                       (gc/circle (v/+polar pos dist (dr/random (* 0.48 eq/TAU) (* 1.02 eq/TAU))) r)
                       :fill (if (dr/chance 0.33) 0.0 1.0))))
       []))))

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

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [debug/display defo]]])

(sketch/definition ballistics
  {:created-at "2023-02-05"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

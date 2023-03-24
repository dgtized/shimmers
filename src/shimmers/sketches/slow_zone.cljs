(ns shimmers.sketches.slow-zone
  "Inspiration is watching coffee grounds in a grinder move rapidly after initial
  kick from the blades and then slowing to join the denser mass of particles.

  Name is a reference to 'Zones of thought' series by Vernor Vinge."
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.verlet-particles :as vp]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn dust-mote [loc]
  (let [start (gv/vec2 loc)
        velocity (gv/vec2 (dr/random 6.0 8.0) 0)]
    (vp/make-particle start (tm/- start velocity) 1.0)))

(defn in-bounds
  [{:keys [pos]} _delta]
  (and (< 0 (:x pos) (q/width))
       (< 0 (:y pos) (q/height))))

(defn slowing-zone [zone]
  (fn [_ {:keys [pos prev]} delta]
    (if (g/contains-point? zone pos)
      (let [change (tm/- prev pos)]
        (if (> (tm/mag-squared change) 2.0)
          (tm/* (gv/vec2 -0.2 0) delta)
          (gv/vec2)))
      (gv/vec2))))

(defn deflection [pos]
  (let [t (q/noise (/ (q/frame-count) 200))
        direction (if (> (:y pos) (* 0.5 (q/height))) -1 1)
        value (cond (< t 0.35) (- t 0.35)
                    (> t 0.65) (- t 0.65)
                    :else 0)]
    (* 0.18 direction value)))

(defn acceleration-zone [zone]
  (fn [_ {:keys [pos]} delta]
    (if (g/contains-point? zone pos)
      (tm/* (gv/vec2 0.2 (deflection pos)) delta)
      (gv/vec2))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [slow (rect/rect (cq/rel-pos 0.2 0.2) (cq/rel-pos 0.6 0.8))
        accel (rect/rect (cq/rel-pos 0.6 0.2) (cq/rel-pos 0.8 0.8))]
    {:system (vp/make-system {:mechanics [(slowing-zone slow)
                                          (acceleration-zone accel)]
                              :constraints [in-bounds]
                              :drag 0.001})}))

(defn update-state [{:keys [system] :as state}]
  (when (and (< (count (:particles system)) 768) (dr/chance 0.66))
    (let [loc (cq/rel-pos 0 (dr/random))]
      (vp/add-particles system (repeatedly (dr/random-int 16) (partial dust-mote loc)))))
  (vp/timestep system 1)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 0.08)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [mote (:particles system)]
    (cq/circle (:pos mote) 1)))

(sketch/defquil slow-zone
  :created-at "2021-08-14"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

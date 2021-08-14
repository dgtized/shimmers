(ns shimmers.sketches.slow-zone
  "Inspiration is watching coffee grounds in a grinder move rapidly after initial
  kick from the blades and then slowing to join the denser mass of particles.

  Name is a reference to 'Zones of thought' series by Vernor Vinge."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.verlet-particles :as vp]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn dust-mote [loc]
  (let [start (gv/vec2 loc)
        velocity (gv/vec2 (tm/random 6.0 8.0) 0)]
    (vp/make-particle start (tm/- start velocity) 1.0)))

(defn in-bounds
  [{:keys [pos]} _delta]
  (and (< 0 (:x pos) (q/width))
       (< 0 (:y pos) (q/height))))

(defn slowing-zone [zone]
  (fn [{:keys [pos prev]} delta]
    (if (geom/contains-point? zone pos)
      (let [change (tm/- prev pos)]
        (if (> (tm/mag-squared change) 1.0)
          (tm/* change 0.2 delta)
          (gv/vec2)))
      (gv/vec2))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:system (vp/make-system {:mechanics [(slowing-zone (rect/rect (cq/rel-pos 0.3 0.2) (cq/rel-pos 0.6 0.8)))]
                            :constraints [in-bounds]
                            :drag 0.001})})

(defn update-state [{:keys [system] :as state}]
  (when (and (< (count (:particles system)) 512) (p/chance 0.8))
    (let [loc (cq/rel-pos 0 (rand))]
      (vp/add-particles system (repeatedly (rand-int 4) (partial dust-mote loc)))))
  (vp/timestep system 2)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 0.05)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [mote (:particles system)]
    (cq/circle (:pos mote) 2)))

(sketch/defquil slow-zone
  :created-at "2021-08-14"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.motion-of-insects
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]
            [shimmers.math.verlet-particles :as vp]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn neighborhood [{at-p :pos :as p} particles radius]
  (filter (fn [{at-q :pos :as q}]
            (and (not= p q) (< (geom/dist at-p at-q) radius)))
          particles))

(defn flock-separation [radius strength]
  (fn [{:keys [particles]} {at-p :pos :as p} delta]
    (let [neighborhood (neighborhood p particles radius)]
      (when (seq neighborhood)
        (let [differences (map (fn [{at-q :pos}]
                                 (tm/div (tm/- at-p at-q) (geom/dist at-p at-q)))
                               neighborhood)
              rel-diff (tm/div (reduce tm/+ differences) (count neighborhood))]
          (tm/* rel-diff (* strength delta)))))))

(defn jumping [distance]
  (fn [_ {:keys [age timing] :as p} _]
    (if (tm/delta= 0.0 (mod age timing))
      (v/jitter distance)
      (gv/vec2))))

(defn make-insect []
  (let [p (cq/rel-vec (tm/random) (tm/random))]
    (assoc (vp/make-particle p (tm/+ p (v/jitter 1.0)) 1.0)
           :timing (int (tm/random 270 720)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:system
   (vp/make-system {:particles (repeatedly 100 make-insect)
                    :mechanics [(flock-separation 36.0 2.0)
                                (jumping 16.0)]
                    :constraints [(vp/wrap-around (q/width) (q/height))]
                    :drag 0.1})})

(defn update-state [{:keys [system] :as state}]
  (vp/timestep system 2)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [pos]} (:particles system)]
    (cq/circle pos 8.0)))

(sketch/defquil motion-of-insects
  :created-at "2021-08-13"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.mechanism
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn pitch-diameter [{:keys [teeth circular-pitch]}]
  ;; alternatively teeth * outside diameter / (teeth + 2)
  (/ (* teeth circular-pitch) Math/PI))

;; https://blog.misumiusa.com/center-to-center-spacing-for-shafts-spur-gears/
(defn center-distance [gear1 gear2]
  (* 0.5 (+ (pitch-diameter gear1) (pitch-diameter gear2))))

(defn gear-ratio [input output]
  (/ (:teeth output) (:teeth input)))

;; https://en.wikipedia.org/wiki/Gear#Spur
;; http://www.gearseds.com/files/Approx_method_draw_involute_tooth_rev2.pdf
(defn tooth [height width p]
  (let [polar (geom/as-polar p)
        qw (/ width 2.2)
        pitch (* height 0.5)]
    (mapv (fn [t] (geom/as-cartesian (tm/+ polar (gv/vec2 t))))
          [[(- pitch) (- width)] [0 (- width)] [height (- qw)]
           [height qw] [0 width] [(- pitch) width]])))

(defn gear [radius teeth]
  (let [points (-> (gc/circle (gv/vec2) radius)
                   (geom/vertices teeth))]
    (gp/polygon2 (mapcat (partial tooth (* radius 0.15) (/ tm/TWO_PI teeth 4)) points))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.001))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (doseq [g [(-> (gear (cq/rel-w 0.061) 8)
                 (geom/rotate t)
                 (geom/translate (gv/vec2 (cq/rel-pos 0.5 0.5))))
             (-> (gear (cq/rel-w 0.08) 10)
                 ;; solve for offset for meshing & change in rate from differential?
                 (geom/rotate (- 0.35 (* t (/ 8 10))))
                 (geom/translate (gv/vec2 (cq/rel-pos 0.35 0.5))))
             (-> (gear (cq/rel-w 0.10) 13)
                 ;; solve for offset for meshing & change in rate from differential?
                 (geom/rotate (- 0.01 (* t (/ 8 13))))
                 (geom/translate (gv/vec2 (cq/rel-pos 0.673 0.5))))]]
    (cq/draw-shape (geom/vertices g))))

(defn ^:export run-sketch []
  ;; 20210419
  (q/defsketch mechanism
    :host "quil-host"
    :size [800 800]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

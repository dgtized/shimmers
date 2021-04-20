(ns shimmers.sketches.mechanism
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
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

;; more useful formula
;; https://www.engineersedge.com/gear_formula.htm
;; https://www.cs.cmu.edu/~rapidproto/mechanisms/chpt7.html

;; https://en.wikipedia.org/wiki/Gear#Spur
;; http://www.gearseds.com/files/Approx_method_draw_involute_tooth_rev2.pdf
(defn tooth [height width p]
  (let [polar (geom/as-polar p)
        qw (/ width 2.2)
        pitch (* height 0.5)]
    (mapv (fn [t] (geom/as-cartesian (tm/+ polar (gv/vec2 t))))
          [[(- pitch) (- width)] [0 (- width)] [height (- qw)]
           [height qw] [0 width] [(- pitch) width]])))

(defn poly-at [polygon pos t]
  (-> polygon
      (geom/rotate t)
      (geom/translate pos)
      geom/vertices))

(defn gear [radius teeth pos t]
  (let [points (-> (gc/circle (gv/vec2) radius)
                   (geom/vertices teeth))]
    {:radius radius
     :teeth teeth
     :shape (-> (gp/polygon2 (mapcat (partial tooth (* radius 0.15) (/ tm/TWO_PI teeth 4)) points))
                (poly-at pos t))
     :angle (-> (gl/line2 (gv/vec2) (gv/vec2 (* 0.66 radius) 0))
                (poly-at pos t))}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.02))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (doseq [g [(gear (cq/rel-w 0.061) 8 (gv/vec2 (cq/rel-pos 0.5 0.5))
                   t)
             ;; how to solve for offset for meshing?
             (gear (cq/rel-w 0.08) 10 (gv/vec2 (cq/rel-pos 0.35 0.5))
                   (- 0.33 (* t (/ 8 10))))
             (gear (cq/rel-w 0.10) 13 (gv/vec2 (cq/rel-pos 0.673 0.5))
                   (- 0 (* t (/ 8 13))))]]
    (q/stroke 0)
    (cq/draw-shape (:shape g))
    (q/stroke 0 0.6 0.6)
    (apply q/line (:angle g))))

(defn ^:export run-sketch []
  ;; 20210419
  (q/defsketch mechanism
    :host "quil-host"
    :size [800 800]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

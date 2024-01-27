(ns shimmers.sketches.phase-shifting
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:parts (g/tessellate (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.25)) 6)
   :table (gv/vec2 (/ 1 2) (/ 1 2))
   :pendulum (gv/vec2 (/ 1 3) (/ 1 3))
   :t 0.0})

(defn update-state [state]
  (update state :t + (/ eq/TAU 60)))

(defn draw [{:keys [parts table pendulum t]}]
  (let [opacity (tm/smoothstep* 0.1 1.0 (eq/unit-sin (+ (* (/ 1 20) t) (Math/sin (* (/ 1 30) t)))))
        origin (cq/rel-vec 0.5 0.5)
        head-amp (eq/unit-sin (+ (* (/ 1 25) t) (* 2 (Math/sin (* (/ 1 35) t)))))
        spin-amp (tm/smoothstep* 0.66 1.0 (eq/unit-sin (+ (* (/ 1 15 t)) (Math/sin (* (/ 1 9) t)))))
        [ftx fty] table
        [fpx fpy] pendulum]
    (q/background 1.0 (* 0.5 opacity))
    (q/stroke-weight 1.0)
    (q/stroke 0.0 (+ 0.15 (* 0.35 opacity)))
    (doseq [polygon parts]
      (let [centroid (g/centroid polygon)
            head (g/heading (tm/- centroid origin))
            table (gv/vec2 (* (cq/rel-h 0.1) (Math/cos (* ftx t)))
                           (* (cq/rel-h 0.1) (Math/sin (* fty t))))
            pos (gv/vec2 (* (cq/rel-h 0.1) (Math/cos (* fpx t)))
                         (* (cq/rel-h 0.1) (Math/sin (* fpy t))))]
        (q/fill (eq/unit-sin (+ (* (/ 1 45) t)
                                (* (/ 1 30) (Math/sin (* (/ 1 11) (/ (+ head t) tm/PHI))))))
                0.5 0.5 (+ 0.05 (* (- 1.0 opacity) 0.05)))
        (-> polygon
            g/center
            (g/scale (+ 0.25 (* 0.75 (eq/unit-sin (+ (* 0.5 head head-amp) (* (/ 1 3) t))))))
            (g/rotate (+ (* t (/ 1 4))
                         (* (/ 3 4) (Math/sin (+ head head-amp (* (/ 1 20) t))))))
            (g/translate (tm/* (tm/- centroid origin)
                               (* 2 (eq/unit-sin (+ (* (/ 1 120) t)
                                                    (Math/sin (* (/ 1 15) t)))))))
            (g/rotate (+ (* t (/ 0.5 tm/PHI))
                         (Math/sin (+ (* 0.2 head spin-amp)
                                      (* (/ 1 60) t)))))
            (g/translate (tm/+ origin
                               (tm/mix (gv/vec2)
                                       (tm/+ pos table)
                                       (tm/smoothstep* 0.33 0.66 (eq/unit-cos (* (/ 1 60) t))))))
            qdg/draw)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition phase-shifting
  {:created-at "2024-01-27"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

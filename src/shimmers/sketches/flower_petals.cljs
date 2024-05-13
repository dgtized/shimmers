(ns shimmers.sketches.flower-petals
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.005))

(defn r-pos [r0 r blades theta]
  (+ r0 (* r (math/cos (* blades theta)))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke 0.0)
  (let [center (cq/rel-vec 0.5 0.5)
        r0 (+ (cq/rel-h 0.1) (* (cq/rel-h 0.05) (math/sin t)))
        r (cq/rel-h 0.35)
        blades (+ 5 (* 4 (math/cos (* 0.1 t))))
        dt 0.05]
    (q/fill 0.0 0.3 0.3)
    ;; (cq/circle (v/+polar center (r-pos r0 r blades t) t) 3)
    (q/no-fill)
    (q/begin-shape)
    (doseq [theta (range 0 (* (inc blades) eq/TAU) dt)]
      (let [p (v/+polar center
                        (r-pos r0 r blades theta)
                        (- t theta))
            [x y] p]
        (q/curve-vertex x y))))
  (q/end-shape))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition flower-petals
  {:created-at "2023-01-09"
   :tags #{:genuary2023}
   :type :quil}
  (ctrl/mount page))

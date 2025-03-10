(ns shimmers.sketches.envelope-collapse
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [clojure.math :as math]
   [thi.ng.math.core :as tm]))

(defn seconds []
  (/ (q/millis) 1000.0))

(defn plot [f res]
  (q/begin-shape)
  (doseq [x (range -0.05 1.05 (/ 1.0 res))]
    (q/vertex (cq/rel-w x) (cq/rel-h (+ (* 0.45 (f x)) 0.5))))
  (q/end-shape))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (seconds)})

(defn update-state [state]
  (assoc state
         :t (seconds)))

;; FIXME: amplitude modulation somewhere here slows down after too many frames
(defn graph [t]
  (let [t (* 0.1 t)]
    (fn [x]
      (let [tx (+ (eq/phase-sin 0.05 t -0.1)
                  (eq/unit-phase-sin 1.15 t (* 0.2 x))
                  (* 0.1 x))
            dtx
            (* 1.5 (eq/phase-sin 0.23 t (* 0.5 x (eq/phase-sin 1.25 t (+ 0.001 x)))))]
        (+ (* (eq/phase-sin 0.66 (+ tx dtx) 0)
              (eq/phase-sin 1.0
                            (+ (* 0.011 x)
                               (* 1.1 (eq/phase-sin -0.051 t (* 0.05 (- x 1.2))))
                               (eq/sqr (eq/phase-sin 0.15 t (eq/phase-sin 0.0001 t (* 0.01 x)))))
                            0.0))
           (* 0.07 (eq/phase-sin 0.005 t (+ (* 7 x) 2.9))))))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/color 0.0)
  (q/stroke-weight 1.0)
  (q/no-fill)
  (let [tan-term (+ (* 0.004 t)
                    (* 1.37 (eq/phase-sin 0.003 t 0.5)))
        offset (* 0.15 (tm/clamp (math/tan (* eq/TAU tan-term)) -100 100))]
    (doseq [v (range -2 3 1)]
      (plot (graph (+ t (* v offset)))
            300))))

(defn page []
  [:div
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition envelope-collapse
  {:created-at "2024-11-17"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

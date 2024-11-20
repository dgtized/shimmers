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

(defn tsin
  ^double [^double r ^double t ^double phase]
  (math/sin (* eq/TAU (+ (* r t) phase))))

(defn utsin
  ^double [^double r ^double t ^double phase]
  (+ 0.5 (* 0.5 (math/sin (* eq/TAU (+ (* r t) phase))))))

;; FIXME: amplitude modulation somewhere here slows down after too many frames
(defn graph [t]
  (let [t (* 0.1 t)]
    (fn [x]
      (let [tx (+ (* t 0.2 (eq/unit-sin (* 2 eq/TAU (+ x (* 0.9 t))))) x)
            dtx (* 1.5 (math/sin (* eq/TAU (+ (* 0.23 t)
                                         (* 1.75 x (math/sin (+ (* 0.1 x) (* 2 t))))))))]
        (+ (* (math/sin (* 1.5 eq/TAU (+ tx dtx)))
              (math/sin
               (* eq/TAU (+ (* 0.011 x)
                       (* 1.1 (math/sin (- x (* 0.21 t) 1.2)))
                       (eq/sqr (tsin 0.15 t (tsin 0.0001 t (* 0.01 x))))))))
           (* 0.07 (math/sin (* eq/TAU (+ (* 7 x) (* 0.005 t) 2.9)))))))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/color 0.0)
  (q/stroke-weight 1.0)
  (q/no-fill)
  (let [tan-term (+ (* 0.007 t)
                    (* 2.1 (math/sin (+ (* eq/TAU 0.003 t) 0.75))))
        offset (* 0.15 (tm/clamp (math/tan (* eq/TAU tan-term)) -100 100))]
    (doseq [v (range -2 3 1)]
      (plot (graph (+ t (* v offset)))
            400))))

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

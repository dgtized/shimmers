(ns shimmers.sketches.bad-circles
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

;; silly rule for the prompt
(def PI 4)

(defn seconds []
  (/ (q/millis) 1000.0))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (seconds)
   :a0 (dr/random PI)
   :a1 (dr/random PI)
   :b0 (dr/random PI)
   :b1 (dr/random PI)})

(defn update-state [state]
  (let [t (seconds)]
    (assoc state :t t)))

(defn draw [{:keys [t a0 a1 b0 b1]}]
  (q/background 1.0)
  (q/stroke-weight 4.0)
  (doseq [theta (map (partial * 2 PI) (tm/norm-range 512))
          :let [h (cq/rel-h 0.5)]]
    (doseq [v (tm/norm-range PI)]
      (let [theta (+ theta (* 0.1 t) v)
            inv-theta (- (* 2 PI) theta)
            a (* 1.5 PI (math/sin (+ (* 0.07 t)
                                     (math/sin (+ (* 0.02 theta) (* 0.03 t) a1))
                                     a0))
                 (+ inv-theta (* 0.125 v) (* 0.002 t)))
            b (* 1.5 PI (math/sin (- (* 0.09 t)
                                     (math/sin (+ (* 0.03 inv-theta) (* 0.04 t) b1))
                                     b0))
                 (+ theta (* 0.125 v) (* 0.002 t)))
            [x y] (-> (cq/rel-vec 0.5 0.5)
                      (v/+polar (* h 0.6) theta)
                      (v/+polar (* h 0.15) a)
                      (v/+polar (* h 0.15) b))]
        (q/point x y)))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div {:style {:width "75ch"}}
    [:p "Genuary2025 Day 17 - What happens if pi=4"]]])

(sketch/definition bad-circles
  {:created-at "2025-01-17"
   :tags #{:genuary2025}
   :type :quil}
  (ctrl/mount page))

(ns shimmers.sketches.bad-circles
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
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
   :a2 (dr/random PI)
   :b0 (dr/random PI)
   :b1 (dr/random PI)
   :b2 (dr/random PI)
   :c0 (dr/random PI)
   :r0 (dr/random PI)})

(defn update-state [state]
  (let [t (seconds)]
    (assoc state :t t)))

(defn ratio [t]
  [(+ 1.0 (* 4.0 (eq/unit-sin t)))
   (+ 1.0 (* 4.0 (eq/unit-cos t)))])

(defn polar-ratio [r theta a b]
  (gv/vec2 (* r (math/cos (* a theta)))
           (* r (math/sin (* b theta)))))

(defn cyclic-mix [a b w t]
  (tm/mix* a b (tm/smoothstep* (- w) w (math/sin t))))

;; Why does one edge grow and loop fast, but the other does not, how to make
;; that grow from the center out?
(defn draw [{:keys [t a0 a1 a2 b0 b1 b2 c0 r0]}]
  (q/background 1.0)
  (q/stroke-weight 4.0)
  (let [len (* PI (+ 1.5 (eq/unit-sin (+ (* 0.01 t) (math/sin (* 0.04 t))))))
        w (* 0.4 (eq/unit-sin (+ (* 0.1 t) (math/sin (+ (* 0.1 len) (* 0.02 t))))))]
    (doseq [theta (map (fn [n] (* len n)) (tm/norm-range 512))
            :let [h (cq/rel-h 0.5)]]
      (doseq [v (tm/norm-range PI)]
        (let [theta (math/pow (* 1 (+ theta (math/pow (math/sin (* 0.125 v t)) 2))) (+ 0.95 w))
              tlen (* 2 (math/sin (* 0.002 t)))
              inv-theta (- (* 2 PI) theta)
              a (* 1.25 PI (math/sin (+ (* 0.19 t)
                                        (math/sin (+ (* 0.02 theta) (* 0.03 t) a1))
                                        a0))
                   (+ inv-theta (* 0.125 v) tlen))
              b (* 1.25 PI (math/sin (+ (* 0.21 t)
                                        (math/sin (+ (* 0.03 inv-theta) (* 0.04 t) b1))
                                        b0))
                   (+ theta (* 0.125 v) tlen))
              ch (* (eq/unit-sin (+ (* PI theta) (* 0.025 t)))
                    (tm/smoothstep* 0.66 1.0
                                    (eq/unit-sin (+ (* 0.01 t) (math/sin (+ theta (* 0.15 t))) c0))))
              c (+ theta (math/sin (+ (* 0.1 v) (* 0.4 t))))
              [cx cy] (ratio (+ (* 0.06 t) r0 (* 0.05 v)))
              [x y] (-> (cq/rel-vec 0.5 0.5)
                        (v/+polar (* h 0.6 (math/pow 0.975 (* w theta))) theta)
                        (v/+polar (* h 0.125)
                                  (cyclic-mix theta a 0.5
                                              (math/sin (+ (* 0.04 t) a2 (math/sin (+ (* 0.1 t) b2))))))
                        (v/+polar (* h 0.125)
                                  (cyclic-mix theta b 0.5
                                              (math/sin (+ (* 0.05 t) b2 (math/sin (+ (* 0.09 t) a2))))))
                        (tm/+ (polar-ratio (* h ch 0.125) c cx cy))
                        (v/+polar (* h ch 0.1) c))]
          (q/point x y))))))

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

(ns shimmers.sketches.wibble-wobble
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
   [thi.ng.math.core :as tm]))

(defn seconds []
  (/ (q/millis) 1000.0))

(defn new-wobble []
  (let [dx (dr/random-tau)
        dy (dr/random-tau)]
    (fn [x y t]
      (* 0.5 (+ (math/sin (+ (- (* 1.5 x) (* 1 t))
                             dx
                             (* 2 (eq/cube (math/sin (+ (* 2.3 y) (* 1.5 t) 5))))))
                (math/sin (+ (- (* 1.2 y) (* 1 t))
                             dy
                             (* 2 (math/sin (+ (* 2.1 x) (* 1.3 t) 5))))))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (seconds)
   :wobble (new-wobble)})

(defn update-state [state]
  (assoc state :t (seconds)))

(defn draw [{:keys [t wobble]}]
  (q/background 1.0 0.2)
  (q/fill 0.6 0.65 0.6 0.5)
  (q/stroke 0.0)
  (q/rect-mode :center)
  (let [cols 90
        rows 60
        w (/ (q/width) cols)
        h (/ (q/height) rows)
        [hw hy] (tm/+ (cq/rel-vec 0.5 0.5)
                      (v/polar (* (cq/rel-h 0.25)
                                  (eq/unit-cos (+ (* 0.9 t)
                                                  (* 2 (eq/unit-sin (* 0.5 t))))))
                               (* 0.5 t)))
        scale (+ 0.0003
                 (* 0.007
                    (+ (eq/unit-cos (* 0.1 t))
                       (eq/unit-sin (+ (eq/unit-cos (/ t tm/PHI)) 1
                                       (* 0.5 t))))))]
    (doseq [i (range cols)
            j (range rows)]
      (let [x (+ (* i w) (* 0.5 w))
            y (+ (* j h) (* 0.5 h))
            m (* 0.5 (+ 1 (wobble (* scale (- x hw)) (* scale (- y hy)) (* 0.5 t))))]
        ;; (q/fill (mod (+ m (* 0.1 t)) 1.0) 0.9 0.6)
        (q/rect x y (* m w) (* m h))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [900 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div
    [:div.readable-width
     [:h4 "Genuary 2024 - Day 13 - Wobbly"]
     [:p "Playing with "
      [:a {:href "https://piterpasma.nl/articles/wobbly"} "Wobbly functions"] "
     on a 2d grid passing through time."]]]])

(sketch/definition wibble-wobble
  {:created-at "2024-01-13"
   :tags #{:genuary2024}
   :type :quil}
  (ctrl/mount page))

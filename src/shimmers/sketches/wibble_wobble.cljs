(ns shimmers.sketches.wibble-wobble
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]))

(defn seconds []
  (/ (q/millis) 1000.0))

(defn new-wobble []
  (let [dx (dr/random-tau)
        dy (dr/random-tau)]
    (fn [x y t]
      (* 0.5 (+ (Math/sin (+ (- (* 1.5 x) (* 1 t))
                             dx
                             (* 2 (eq/cube (Math/sin (+ (* 2 y) (* 1.5 t) 5))))))
                (Math/sin (+ (- (* 1.2 y) (* 1 t))
                             dy
                             (* 2 (Math/sin (+ (* 2 x) (* 1.3 t) 5))))))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (seconds)
   :wobble (new-wobble)})

(defn update-state [state]
  (assoc state :t (seconds)))

(defn draw [{:keys [t wobble]}]
  (q/background 1.0 0.1)
  (q/no-fill)
  (q/stroke 0.0)
  (q/rect-mode :center)
  (let [cols 90
        rows 60
        w (/ (q/width) cols)
        h (/ (q/height) rows)]
    (doseq [i (range cols)
            j (range rows)]
      (let [x (+ (* i w) (* 0.5 w))
            y (+ (* j h) (* 0.5 h))
            m (* 0.5 (+ 1 (wobble (* 0.01 x) (* 0.01 y) (* 0.5 t))))]
        (q/rect x y
                (* m w) (* m h))))))

(defn page []
  [:div
   (sketch/component
     :size [900 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition wibble-wobble
  {:created-at "2024-01-13"
   :tags #{:genuary2024}
   :type :quil}
  (ctrl/mount page))

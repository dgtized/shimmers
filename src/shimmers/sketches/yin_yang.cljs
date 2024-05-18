(ns shimmers.sketches.yin-yang
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn sum-square [r1 r2]
  (+ (* r1 r1) (* r2 r2)))

(defn setup []
  (q/color-mode :hsl 255 255 255 255)
  (let [r-size (cq/rel-h 0.125)]
    {:direction 0.001
     :r-size r-size
     :r1 r-size
     :r2 r-size}))

(defn migrate-volume [{:keys [direction r1 r2 r-size] :as state}]
  (let [rN (+ r1 (* direction r2))]
    (assoc state
           :r1 rN
           :r2 (math/sqrt (- (sum-square r-size r-size) (* rN rN))))))

(defn update-state [{:keys [r1 r2] :as state}]
  (if (or (< r1 10) (< r2 10))
    (migrate-volume (update state :direction * -1))
    (migrate-volume state)))

(defn draw [{:keys [r1 r2]}]
  (q/ellipse-mode :radius)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [fc (q/frame-count)
        sc (/ fc 20)]
    (when (#{0 1 3 6 10 15} (mod fc 19))
      (q/rotate (/ fc 180))
      (if (even? fc)
        (q/fill 255 255 255 10)
        (q/no-fill))
      (q/stroke (mod (+ sc r1) 255) 100 140 48)
      (q/ellipse (- r1) 0 r1 r1)
      (q/stroke (mod (- sc r2) 255) 100 140 48)
      (q/ellipse r2 0 r2 r2))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition yin-yang
  {:created-at "2021-02-03"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

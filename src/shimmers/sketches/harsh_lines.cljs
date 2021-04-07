(ns shimmers.sketches.harsh-lines
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.line :as gl]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [kixi.stats.distribution :as ksd]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:lines (for [y (range 0.2 0.8 0.1)]
            (gl/line2 (cq/rel-pos 0.1 y) (cq/rel-pos 0.9 y)))})

(defn update-state [state]
  state)

(defn verticle-line [line t]
  (let [p (geom/point-at line t)]
    (-> (gl/line2 [0 -1] [0 1])
        (geom/rotate (ksd/draw (ksd/normal {:mu 0 :sd 0.1})))
        (geom/scale-size (cq/rel-h 0.035))
        (geom/translate p))))

(defn draw [{:keys [lines]}]
  (q/stroke-weight 0.3)
  (doseq [line lines]
    (let [dx 0.004]
      (doseq [x (range 0 1 dx)]
        (let [t (+ x (* dx (rand)))
              {[p q] :points} (verticle-line line t)]
          (q/line p q))))))

(defn ^:export run-sketch []
  ;; 20210407
  (q/defsketch harsh-lines
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

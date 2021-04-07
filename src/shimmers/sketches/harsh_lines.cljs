(ns shimmers.sketches.harsh-lines
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:lines (for [y (range 0.2 0.8 0.1)]
            (gl/line2 (cq/rel-pos (q/random 0.05 0.4) y)
                      (cq/rel-pos (q/random 0.6 0.95) y)))})

(defn verticle-line [line t angle-sd]
  (let [p (geom/point-at line t)]
    (-> (gl/line2 [0 -1] [0 1])
        (geom/rotate (ksd/draw (ksd/normal {:mu 0 :sd angle-sd})))
        (geom/scale-size (cq/rel-h 0.035))
        (geom/translate p))))

(defn draw [{:keys [lines]}]
  (q/stroke-weight 0.3)
  (doseq [[i line] (map-indexed vector lines)]
    (let [dx 0.004
          normal (ksd/normal {:mu 0 :sd dx})]
      (doseq [x (range 0 1 dx)]
        (let [t (+ x (ksd/draw normal))
              {[p q] :points} (verticle-line line t (* 0.03 (inc i)))]
          (q/line p q))))))

(defn ^:export run-sketch []
  ;; 20210407
  (q/defsketch harsh-lines
    :host "quil-host"
    :size [600 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))

(ns shimmers.sketches.harsh-lines
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]))

(defn gaussian [mu sd]
  (ksd/draw (ksd/normal {:mu mu :sd sd})))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:lines (for [y (range 0.2 0.8 0.1)]
            (gl/line2 (cq/rel-pos (q/random 0.05 0.35) y)
                      (cq/rel-pos (q/random 0.65 0.95) (gaussian y 0.02))))})

(defn verticle-line [line t height-sd angle-sd]
  (let [p (geom/point-at line t)]
    (-> (gl/line2 [0 -1] [0 1])
        (geom/rotate (gaussian 0 angle-sd))
        (geom/scale-size (gaussian (cq/rel-h 0.025) height-sd))
        (geom/translate p))))

(defn draw [{:keys [lines]}]
  (q/stroke-weight 0.3)
  (doseq [[i line] (map-indexed vector lines)]
    (let [dx 0.003]
      (doseq [x (range 0 1 dx)]
        (let [t (+ x (gaussian 0 (* x dx)))
              {[p q] :points}
              (verticle-line line t
                             (* x (* 0.2 (inc i)) (cq/rel-h 0.01))
                             (* 0.03 (* t (inc i))))]
          (q/line p q))))))

(defn ^:export run-sketch []
  ;; 20210407
  (q/defsketch harsh-lines
    :host "quil-host"
    :size [900 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))

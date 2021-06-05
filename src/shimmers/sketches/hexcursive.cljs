(ns shimmers.sketches.hexcursive
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.quil :as cq]
            [shimmers.math.hexagon :as hex]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]))

(defn subdivide [depth {:keys [p r] :as h}]
  (if (> depth 8)
    [h]
    (let [r' (/ r 3)
          children (mapv (fn [x] (-> (hex/axial->cube x)
                                    (hex/cube-hexagon r')
                                    (geom/translate p)))
                         (hex/cube-spiral (gv/vec2) 1))]
      (concat [h]
              (take depth children)
              (mapcat subdivide (map #(+ depth 1 %) (range 0 8))
                      (drop depth children))))))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  (let [r (* (/ 0.99 (Math/sqrt 3)) (q/height))]
    {:shapes (subdivide 0 (hex/hexagon (gv/vec2) r))}))

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.4)
  (q/no-fill)
  (q/translate (cq/rel-pos 0.5 0.5))
  (doseq [a-hex shapes]
    (-> a-hex
        (geom/vertices 6)
        cq/draw-shape)))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch hexcursive
    :host "quil-host"
    :size [800 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))

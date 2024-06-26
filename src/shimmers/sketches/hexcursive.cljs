(ns shimmers.sketches.hexcursive
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn color [hex i]
  (assoc hex :color
         [(if (odd? i) 0.45 0.55)
          0.65
          (tm/map-interval (/ i 7) 0.0 1.0 0.55 0.95)
          0.8]))

(defn subdivide [depth {:keys [p r] :as h}]
  (if (> depth 8)
    [h]
    (let [children
          (map-indexed (fn [i x] (-> x
                                    (hex/cube-hexagon (/ r 3))
                                    (g/translate p)
                                    (color (mod (+ i depth) 7))))
                       (hex/cube-spiral (gv/vec3) 1))]
      (concat [h]
              (take depth children)
              (mapcat subdivide (map #(+ depth %) (range 1 8))
                      (drop depth children))))))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  (let [r (* (/ 0.99 tm/SQRT3) (q/height))]
    {:shapes (subdivide -1
                        (assoc (hex/hexagon (gv/vec2) r) :color [1.0 1.0]))}))

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.3)
  (q/no-fill)
  (q/translate (cq/rel-pos 0.5 0.5))
  (doseq [a-hex shapes]
    (apply q/fill (:color a-hex))
    (-> a-hex
        (g/vertices 6)
        cq/draw-shape)))

(defn page []
  (sketch/component
   :size [1200 1000]
   :setup setup
   :draw draw
   :middleware [m/fun-mode]))

;; TODO: convert to svg
(sketch/definition hexcursive
  {:created-at "2021-06-04"
   :tags #{:static}
   :type :quil}
  (ctrl/mount page))

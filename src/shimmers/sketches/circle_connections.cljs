(ns shimmers.sketches.circle-connections
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]))

(defn vertices [{:keys [p r]} n theta]
  (for [i (range n)]
    (v/+polar p r (* eq/TAU (+ (/ i n) theta)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (assoc state :t (/ (q/millis) 1000.0)))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/fill 1.0)

  (let [circles [(gc/circle (cq/rel-vec 0.10 0.5) (cq/rel-w 0.075))
                 (gc/circle (cq/rel-vec 0.92 0.5) (cq/rel-w 0.075))]
        [c0 c1] circles
        n 24
        v0 (vertices c0 n (* 0.075 Math/PI (Math/sin (* t 0.3))))
        v1 (vertices c1 n (* 0.075 Math/PI (Math/cos (* t 0.4))))]
    (q/stroke-weight 1.25)
    (doseq [c circles]
      (qdg/draw c))

    (q/stroke-weight 0.5)
    (doseq [[p0 p1] (map vector v0 v1)]
      (q/line p0 p1))

    (doseq [p (concat v0 v1)]
      (qdg/draw (gc/circle p 2.0)))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition circle-connections
  {:created-at "2023-07-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

(ns shimmers.sketches.intersection-brushes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.core :as sm]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Trying out the technique Casey Reas described in
;; https://www.youtube.com/watch?v=_8DMEHxOLQE, ie move circles through space,
;; and draw a line between centers if circle intersects.

(defn color-mix [a b]
  (let [r (:r b)
        t (/ r (+ (:r a) r))
        [hueA & v1] (:color a)
        [hueB & v2] (:color b)]
    (into [(sm/mix-mod hueA hueB t)]
          (mapv #(tm/mix* %1 %2 t) v1 v2))))

(defn intersecting [circles]
  (for [[a b] (cs/all-pairs circles)
        :when (geom/intersect-shape a b)]
    [a b]))

(defn make-circle [hue rand-velocity]
  (let [r (+ 0.01 (* 0.03 (rand)))
        x (q/random r (- 1 r))
        y (q/random r (- 1 r))]
    (assoc (gc/circle x y r)
           :velocity (tm/* (rand-velocity) (* (rand) 0.001))
           :color [(mod (+ hue (* 0.1 (q/random-gaussian))) 1.0)
                   (q/random 0.4 0.8)
                   (q/random 0.4 0.6)
                   0.02])))

(defn reflect-boundary [{:keys [p velocity] :as circle} bounds]
  (if (geom/contains-point? bounds p)
    circle
    (let [close (geom/closest-point bounds p)
          hit-y-axis (#{0 1} (:x close))
          reflected (update velocity (if hit-y-axis :x :y) -)]
      (assoc circle :velocity reflected
             :p (tm/+ close reflected)))))

(defn update-positions [circles]
  (for [{:keys [velocity] :as circle} circles]
    (-> circle
        (update :p tm/+ velocity)
        (reflect-boundary (rect/rect)))))

(defn random-cardinal []
  (rand-nth [(gv/vec2 1 0) (gv/vec2 -1 0) (gv/vec2 0 1) (gv/vec2 0 -1)]))

(defn random-diagonal []
  (rand-nth [(gv/vec2 0.5 0.5) (gv/vec2 -0.5 0.5) (gv/vec2 0.5 -0.5) (gv/vec2 -0.5 -0.5)]))

(defn random-hexagon []
  (rand-nth (for [theta (range 0 (* 2 Math/PI) (/ Math/PI 3))] (geom/as-cartesian (gv/vec2 1 theta)))))

(defn velocity-seed
  "Generates starting velocities according to some common randomized approach"
  []
  (rand-nth [random-cardinal random-diagonal random-hexagon gv/randvec2]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1)
  {:color (p/chance 0.8)
   :circles (repeatedly 128 (partial make-circle (rand) (velocity-seed)))})

(defn update-state [state]
  (update state :circles update-positions))

(defn draw [{:keys [color circles]}]
  ;; (q/background 1.0 1.0 1.0 1.0)
  ;; (q/ellipse-mode :radius)
  ;; (doseq [{:keys [p r]} circles
  ;;         :let [[x y] (cq/rel-pos p)
  ;;               radius (cq/rel-h r)]]
  ;;   (q/ellipse x y radius radius))

  (q/stroke-weight 0.5)
  (q/stroke 0 0.01)
  (doseq [[a b] (intersecting circles)]
    (let [pa (cq/rel-pos (:p a))
          pb (cq/rel-pos (:p b))]
      (when color
        (apply q/stroke (color-mix a b)))
      (q/line pa pb))))

(defn ^:export run-sketch []
  (q/defsketch intersection-brushes
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

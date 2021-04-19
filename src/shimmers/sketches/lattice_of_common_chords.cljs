(ns shimmers.sketches.lattice-of-common-chords
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.core :as sm]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
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

;; Something is either funky with the draw radius or intersection as it looks
;; like it starts drawing a line when they are a not yet intersecting. It
;; doesn't break the effect but looks odd in debug. Possibly just related to
;; aspect ratio?

;; TODO: Can we avoid N^2 / 2 compares using a quadtree or some kind of regional
;; optimization?
(defn intersecting [circles]
  (for [[a b] (cs/all-pairs circles)
        :when (geom/intersect-shape a b)]
    [a b]))

;; TODO: set initial positions with matching velocities?
;; ie everyone starts on line Y and goes up or down or something?
(defn make-circle [{:keys [position radius hue rand-velocity]}]
  (let [r (+ 0.01 (* radius (rand)))
        [x y] (position r)]
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

(defn position-seed
  []
  (p/weighted
   {(fn [_] (geom/random-point (gc/circle 0.5 0.5 0.2))) 1
    (let [[y0 y1] (repeatedly 2 #(rand-nth [0.35 0.5 0.65]))
          line (gl/line2 0.1 y0 0.9 y1)]
      (fn [_] (geom/random-point line))) 2
    (let [[x0 x1] (repeatedly 2 #(rand-nth [0.4 0.5 0.4]))
          line (gl/line2 x0 0.1 x1 0.9)]
      (fn [_] (geom/random-point line))) 1
    (fn [r] (repeatedly 2 #(+ 0.25 (* 0.5 (rand))))) 1
    (fn [r] (repeatedly 2 #(q/random r (- 1 r)))) 1.5}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/background 1)
  (let [rules {:hue (rand)
               :radius (rand-nth [0.02 0.03 0.03 0.06])
               :position (position-seed)
               :rand-velocity (velocity-seed)}]
    {:color (p/chance 0.66)
     :circles (repeatedly 96 (partial make-circle rules))}))

(defn update-state [state]
  (cq/if-steady-state state 10 setup
                      (fn [state] [(= 0 (mod (q/frame-count) (* 1 60 60)))
                                  (update state :circles update-positions)])))

(defonce ui-state (ctrl/state {:debug false :sand false}))
(defn explanation []
  [:div
   (ctrl/checkbox ui-state "Debug" [:debug])
   (ctrl/checkbox ui-state "Sand" [:sand])])

(defn sand-line [pa pb]
  (q/stroke-weight 1)
  (let [line (gl/line2 pa pb)
        r 0.1]
    (doseq [p (ksd/sample 10 (ksd/uniform {:b 1}))
            :let [[x y] (geom/point-at line p)]]
      (q/ellipse x y r r))))

(defn draw [{:keys [color circles]}]
  (q/stroke-weight 0.5)
  (if (:debug @ui-state)
    (do (q/background 1.0 1.0 1.0 1.0)
        (q/stroke 0 1)
        (doseq [{:keys [p r]} circles
                :let [[x y] (cq/rel-pos p)
                      radius (cq/rel-h r)]]
          (q/ellipse x y radius radius)))
    (q/stroke 0 0.01))

  (doseq [[a b] (intersecting circles)]
    (let [pa (cq/rel-pos (:p a))
          pb (cq/rel-pos (:p b))]
      (when (and color (not (:debug @ui-state)))
        (apply q/stroke (color-mix a b)))
      (if (:sand @ui-state)
        (sand-line pa pb)
        (q/line pa pb)))))

(defn ^:export run-sketch []
  ;; 20210329
  (ctrl/mount explanation)
  (q/defsketch lattice-of-common-chords
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

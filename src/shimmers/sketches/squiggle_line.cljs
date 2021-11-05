(ns shimmers.sketches.squiggle-line
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn random-point-in-circle [r]
  (-> (gv/vec2
       (* r (Math/sqrt (dr/random-double)))
       (* tm/TWO_PI (dr/random-double)))
      g/as-cartesian))

;; Implementing squiggly line drawing as described in
;; https://rmarcus.info/blog/2017/10/23/humanlines.html
;; https://rmarcus.info/blog/assets/humanLines/Meraj08.pdf
;; https://github.com/RyanMarcus/humanLines/blob/master/index.js

(defn squiggle-poly [p q t]
  (let [tau (/ t 2.0)
        term (- (* 15 (Math/pow tau 4))
                (* 6 (Math/pow tau 5))
                (* 10 (Math/pow tau 3)))]
    ;; Seems safer to use abs, but something weird is flipped with mix here?
    (tm/mix p q (Math/abs term))))

(defn squiggle-points [p q]
  (let [dist (g/dist p q)
        dt (cond (< dist 200) 0.5
                 (< dist 400) 0.3
                 :else 0.2)]
    (map (partial squiggle-poly p q) (range 0 2 dt))))

(defn squiggle-control [p q]
  (let [midpoint (tm/mix p q 0.5)
        displacement (random-point-in-circle 5)] ;; this should be biased toward normal at point?
    (tm/+ midpoint displacement)))

;; TODO: add controls for dt and displacement quantity
(defn squiggle [p q]
  (q/begin-shape)
  (apply q/vertex p)
  (doseq [[prev current] (partition 2 1 (squiggle-points p q))
          :let [[x y] current
                [cx cy] (squiggle-control prev current)]]
    (q/quadratic-vertex cx cy x y))
  (q/end-shape))

(defn rel-line [{[p q] :points}]
  (gl/line2 (cq/rel-vec p) (cq/rel-vec q)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:lines (mapv rel-line [(gl/line2 0.2 0.2 0.8 0.2)
                          (gl/line2 0.3 0.3 0.6 0.3)
                          (gl/line2 0.7 0.25 0.7 0.6)
                          (gl/line2 0.8 0.35 0.8 0.8)
                          (gl/line2 0.2 0.7 0.5 0.4)
                          (gl/line2 0.5 0.5 0.6 0.7)])})

(defn update-state [state]
  state)

(defn draw [{:keys [lines]}]
  (q/background 1.0)
  (doseq [{[p q] :points} lines]
    (squiggle p q)))

(sketch/defquil squiggle-line
  :created-at "2021-"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.algorithm.hand-drawn
  (:require
   [quil.core :as q :include-macros true]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.core :as g]
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

(defn control-points [p q]
  (let [dist (g/dist p q)
        dt (cond (< dist 200) 0.5
                 (< dist 400) 0.3
                 :else 0.2)]
    (map (partial squiggle-poly p q) (range 0 2 dt))))

(defn deviation [prev current]
  (let [midpoint (tm/mix prev current 0.5)
        displacement (random-point-in-circle 5)] ;; this should be biased toward normal at point?
    (tm/+ midpoint displacement)))

;; TODO: add controls for dt and displacement quantity
(defn squiggle [p q]
  (q/begin-shape)
  (apply q/vertex p)
  (doseq [[prev current] (partition 2 1 (control-points p q))
          :let [[x y] current
                [cx cy] (deviation prev current)]]
    (q/quadratic-vertex cx cy x y))
  (q/end-shape))

(defn line [p q]
  (squiggle p q))



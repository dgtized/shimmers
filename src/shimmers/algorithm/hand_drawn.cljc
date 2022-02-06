(ns shimmers.algorithm.hand-drawn
  (:require
   [quil.core :as q :include-macros true]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; See also: https://roughjs.com/

(defn random-point-in-circle [r]
  (-> (gv/vec2
       (* r (Math/sqrt (dr/random-double)))
       (* tm/TWO_PI (dr/random-double)))
      g/as-cartesian))

;; Implementing squiggly line drawing as described in
;; https://rmarcus.info/blog/2017/10/23/humanlines.html
;; https://rmarcus.info/blog/assets/humanLines/Meraj08.pdf
;; https://github.com/RyanMarcus/humanLines/blob/master/index.js
(defn control-points [p q]
  (let [dist (g/dist p q)
        dt (cond (< dist 200) 0.5
                 (< dist 400) 0.3
                 :else 0.2)]
    ;; norm-range is inclusive of the last position
    (for [t (tm/norm-range (/ 2.0 dt))]
      (let [term (- (* 15 (Math/pow t 4))
                    (* 6 (Math/pow t 5))
                    (* 10 (Math/pow t 3)))]
        ;; Seems safer to use abs, but something weird is flipped with mix here?
        (tm/mix p q (Math/abs term))))))

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

(defn strip [vertices]
  (doseq [[p q] (partition 2 1 vertices)]
    (line p q)))

(defn closed-strip [vertices]
  (strip vertices)
  (line (last vertices) (first vertices)))

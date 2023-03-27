(ns shimmers.sketches.amplification
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn distance-to-edge [bounds p]
  (g/dist p (g/closest-point bounds p)))
(partition 2 2 (range 10))

(defn skip-line [a b]
  (let [n (dr/random-int 12 48)]
    (->> (tm/norm-range n)
         (drop 1)
         (partition 2 3)
         (mapcat (fn [[t0 t1]] [[:M (tm/mix a b t0)]
                               [:L (tm/mix a b t1)]]))
         csvg/path)))

(defn arc-segment [pos r t0 t1]
  (let [src (v/+polar pos r t0)
        dest (v/+polar pos r t1)]
    [[:M src]
     [:A [r r] 0.0
      (if (> (Math/abs (- t1 t0)) Math/PI) 1 0)
      (if (> t1 t0) 1 0)
      dest]]))

(defn segmented-circle [pos r]
  (let [n (Math/ceil (* 64 (dr/circular-random)))
        base (dr/random-tau)]
    (if (<= n 1)
      (gc/circle pos r)
      (->> (tm/norm-range n)
           (partition 2 1)
           (take-nth 2)
           (mapcat (fn [[s0 s1]]
                     (arc-segment pos
                                  (* r (dr/gaussian 1.0 0.01))
                                  (+ base (* s0 eq/TAU))
                                  (+ base (* s1 eq/TAU)))))
           csvg/path))))

(defn sketch-circle [pos r]
  (let [n (Math/ceil (* 8 (dr/circular-random)))]
    (map (fn [_]
           (if (dr/chance 0.66)
             (gc/circle (tm/+ pos (dr/jitter 4.0)) r)
             (segmented-circle pos r)))
         (range n))))

(defn make-concentric [pos max-radius offsets]
  (mapcat (fn [o] (sketch-circle pos (* max-radius o)))
          offsets))

(defn shapes [bounds]
  (let [center (rv (dr/random 0.25 0.75) (dr/random 0.35 0.65))
        close-edge-point (g/closest-point bounds center)
        edge-dist (g/dist center close-edge-point)]
    (concat (make-concentric center
                             (* 0.66 edge-dist)
                             (drop 1 (tm/norm-range 5)))
            (mapcat (fn [t]
                      (let [direction (dr/gaussian (+ (* 0.75 eq/TAU) (* eq/TAU t)) 0.2)
                            proj (v/+polar center
                                           (+ (* (- 1 (eq/cos-similarity (tm/- close-edge-point center)
                                                                         (v/polar 1 direction)))
                                                 (* 0.5 edge-dist))
                                              (* 0.6 edge-dist))
                                           direction)
                            proj-edge-dist (distance-to-edge bounds proj)]
                        (when (g/contains-point? (g/scale-size bounds 0.9) proj)
                          (concat (make-concentric proj
                                                   (* 0.4 proj-edge-dist)
                                                   (drop 1 (tm/norm-range 3)))
                                  [(skip-line center proj)]))))
                    (drop 1 (tm/norm-range (dr/random-int 2 8)))))))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition amplification
  {:created-at "2023-03-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :amplification)
              "sketch-host"))

(ns shimmers.sketches.radial-mosaic
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.strf.core :as f]))

(def width 900)
(def height 600)
(defn r [x y]
  (gv/vec2 (* x width) (* y height)))

(defn polar [r theta]
  (geom/as-cartesian (gv/vec2 r theta)))

(defn radial-range [dt]
  (let [r (range 0 tm/TWO_PI dt)]
    (conj (vec (map vec (partition 2 1 r))) [(last r) (first r)])))

(defn first-last [coll]
  [(first coll) (last coll)])

(defn partition-segments [chunks pads coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [n (first chunks)
           p (take n s)]
       (if (== n (count p))
         (cons (first-last p)
               (partition-segments (rest chunks) (rest pads)
                                   (drop (+ n (first pads)) s)))
         (list (first-last (take n p))))))))

(comment (partition-segments #(int (tm/random 4 24))
                             #(int (tm/random 0 4))
                             (range 100))
         (partition-segments (cycle [4 8 16])
                             (cycle [1 2])
                             (range 100)))

(defn segment [origin t0 t1 r0 r1]
  (let [[x0 y0] (tm/+ origin (polar r0 t0))
        [x1 y1] (tm/+ origin (polar r1 t1))]
    (svg/path [[:M (tm/+ origin (polar r0 t0))]
               [:L (tm/+ origin (polar r1 t0))]
               #_[:L (tm/+ origin (polar r1 t1))]
               [:A [r1 r1] 0.0 0 1 [x1 y1]]
               [:L (tm/+ origin (polar r0 t1))]
               #_[:L (tm/+ origin (polar r0 t0))]
               [:A [r0 r0] 0.0 0 0 [x0 y0]]
               [:Z]]
              {:fill "none"
               :stroke-width 0.6
               :stroke "black"
               :key (str "s:" t0 "-" t1 "-" r0)})))

(comment
  (f/format (:A svg/path-segment-formats) (gv/vec2 0.5 0.1) 1.0 1.0 1.0 (gv/vec2 1.0 0.5))
  (f/format [(f/float 2)] 0.21)
  (segment (gv/vec2) 0.5 1 1 2))

(defn scene [origin]
  (csvg/svg {:width width :height height}
            (gc/circle origin 10)
            (mapcat (fn [[[r0 r1] segments st]]
                      (for [[t0 t1] (radial-range (/ tm/TWO_PI segments))]
                        (segment origin (+ st t0) (+ st t1) r0 r1)))
                    (map vector
                         (partition-segments (repeatedly #(int (tm/random 10 30)))
                                             (repeatedly #(int (tm/random 1 3)))
                                             (range 11 (int (* 0.5 height))))
                         (repeatedly #(int (tm/random 16 32)))
                         (repeatedly #(tm/random 0.0 0.2))))))

(defn page []
  [:div (scene (r 0.5 0.5))])

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

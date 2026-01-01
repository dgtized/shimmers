(ns shimmers.sketches.connect-and-fill
  (:require
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.string :as scs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.color.core :as col]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn displace [bounds p]
  (let [radius (* 0.5 (min width height))]
    (->>
     (fn [] (v/+polar p (dr/random (* 0.15 radius) (* 0.75 radius))
                     (dr/random-tau)))
     repeatedly
     (some
      (fn [p]
        (when (g/contains-point? bounds p)
          p))))))


(defn point-triplets [pts]
  (partition 3 1 (concat pts (take 2 pts))))

(comment
  (point-triplets (vec (range 4))))

(defn acceptable? [pts]
  (every?
   (fn [[a b c]]
     (let [angle (poly-detect/small-angle-between (tm/- a b) (tm/- c b))]
       (> angle (* eq/TAU 0.05))))
   (point-triplets pts)))

(defn points [bounds n]
  (->>
   (fn []
     (take n
           (iterate (fn [p] (displace (g/scale-size bounds 0.7) p))
                    (rp/sample-point-inside (g/scale-size bounds 0.3)))))
   repeatedly
   (some
    (fn [pts] (when (acceptable? (vec pts)) pts)))))

(defn debug-points [pts]
  (mapv (fn [[a b c]]
          (csvg/group {}
            (vary-meta (gc/circle b 3.0)
                       assoc
                       :fill (let [angle (poly-detect/small-angle-between (tm/- a b) (tm/- c b))]
                               (if (< angle (* eq/TAU 0.05))
                                 "red" "green")))
            (svg/text (tm/+ b (gv/vec2 5.0 0.0))
                      (apply scs/cl-format "[~0,1f, ~0,1f]" b))))
        (point-triplets pts)))

;; g/centroid for polygon assumes non-self-intersecting polygon
(defn simple-centroid [shape]
  (let [pts (g/vertices shape)]
    (tm/div (reduce tm/+ pts) (count pts))))

(defn shape-set [bounds n]
  (let [pts (vec (points bounds n))
        shape (gp/polygon2 pts)
        copies (dr/rand-nth [3 5 7])
        cshape (g/translate shape (tm/- (g/centroid bounds) (simple-centroid shape)))
        displacement
        (tm/* (gv/vec2 (/ (g/width bounds) 5)
                       (/ (g/height bounds) 5))
              (dr/weighted {(gv/vec2 1 0) 1
                            (gv/vec2 0 1) 1
                            (gv/vec2 1 (* -1 (dr/random 0.75 1.0))) 1
                            (gv/vec2 (* -1 (dr/random 0.75 1.0)) 1) 1}))
        translations
        (dr/weighted
         {(mapv (fn [t] (v/polar (/ (g/height bounds) 3.5) (* eq/TAU t)))
                (butlast (tm/norm-range copies))) 1.0
          (mapv (fn [x] (tm/* displacement (* 2 (- x 0.5))))
                (tm/norm-range (dec copies))) 1.0})]
    {:shape cshape
     :copies (mapv
              (fn [s t] (g/translate s t))
              (repeat copies cshape)
              translations)}))

(defn legal? [bounds]
  (fn [{:keys [copies] :as set}]
    (when (every? (fn [p] (g/contains-point? bounds p))
                  (mapcat g/vertices copies))
      set)))

(defn shapes [bounds]
  (let [color (col/as-css (col/hsla (dr/random) 0.8 0.5 0.225))
        set (some (legal? (g/scale-size bounds 0.95))
                  (repeatedly #(shape-set bounds 7)))]
    (concat #_(debug-points (g/vertices (:shape set)))
            (mapv
             (fn [s]
               (vary-meta s assoc :fill color))
             (:copies set)))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "none"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes (g/scale-size (csvg/screen width height) 0.9))))

(defn explanation [_]
  [:div.readable-width
   [:p "Genuary 2026 - Day 01 - One Color, One Shape"]
   [:p "Generate a random 8 vertice shape and then make " [:em "n"] " copies of it arranged in different patterns."]])

(sketch/definition connect-and-fill
  {:created-at "2026-01-01"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))

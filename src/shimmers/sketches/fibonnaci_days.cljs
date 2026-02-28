(ns shimmers.sketches.fibonnaci-days
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(comment
  (= 12 (+ 1 1 2 3 5))
  (= 20 (+ 1 1 2 3 5 8))
  (= 33 (+ 1 1 2 3 5 8 13)))

(defn fib
  ([] (fib 1 1))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(comment (take 7 (fib)))

(defn cuts [n]
  (let [xs (take n (fib))
        ss (reductions + xs)
        total (last ss)]
    (mapv (fn [x] (/ x total)) ss)))

(defn arc-segment [p q right]
  (let [mid (tm/mix p q 0.5)
        r (g/dist p mid)]
    (csvg/path [[:M p]
                [:A [r r] 0.0 0 (if right 0 1) q]])))

;; FIXME: ignoring the arc length for the arc segments in both distance traveled
;; and in mapping positions on the number line.
(defn point-at [lines t]
  (let [n (count lines)
        st (* n t)]
    (cond (tm/delta= 0.0 t) (g/point-at (first lines) 0.0)
          (tm/delta= 1.0 t) (g/point-at (last lines) 1.0)
          :else
          (g/point-at (nth lines (int st))
                      (- st (int st))))))

(defn shapes []
  (let [lines (cs/midsection (tm/norm-range (inc 13)))
        path
        (map-indexed
         (fn [i t]
           (if (odd? i)
             (gl/line2 (rv 0.8 t) (rv 0.2 t))
             (gl/line2 (rv 0.2 t) (rv 0.8 t))))
         lines)]
    (concat path
            (map-indexed
             (fn [i [a b]]
               (let [x (if (odd? i) 0.2 0.8)]
                 (arc-segment (rv x a) (rv x b) (odd? i))))
             (partition 2 1 lines))
            (map (fn [t]
                   (gc/circle (point-at path t) 6))
                 (cuts 10)))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 2}
    (shapes)))

(defn explanation []
  [:div
   [:p "Genuary 2026 - Day3 - Fibonnaci forever"]
   [:p "Concept was to plot each bubble proportional to the fibonnaci sequence,
   ie proportional spacing to the golden ratio."]])

(sketch/definition fibonnaci-days
  {:created-at "2026-01-03"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))

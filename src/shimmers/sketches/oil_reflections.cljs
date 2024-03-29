(ns shimmers.sketches.oil-reflections
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds (cq/screen-rect 0.9)
   :circles []})

(defn remove-middle [hatches n]
  (let [c (count hatches)
        sorted (sort-by (fn [{[[x y] _] :points}] [y x]) hatches)
        edges (int (- (/ c 2) (/ n 2)))]
    (into (take edges sorted)
          (take-last edges sorted))))

;; TODO: namespace for line segmentation tricks like this and stroke-line as
;; well as for functions operating on a list of lines.
(defn sketch-line [{[p q] :points}]
  (let [edge-p 0.02
        edge-q 0.02
        chunks (inc (rand-int 8))
        jitter (* 0.02 (/ (g/dist p q) chunks))]
    (for [[a b] (partition 2 1 (concat [(- edge-p)] (cs/centered-range chunks) [(+ 1 edge-q)]))]
      (gl/line2 (rp/confusion-disk (tm/mix p q (- a (dr/random edge-p))) jitter)
                (rp/confusion-disk (tm/mix p q (+ b (dr/random edge-q))) jitter)))))

;; TODO: add recursive split line with declining likelyhood to recursion depth.
;; not clear how to keep outer/inner parameters particularly as each segment can
;; subdivide and start overlapping.
(defn stroke-line [outer inner {[p q] :points}]
  (let [a (dr/random (- outer) inner)
        b (dr/random (- 1.0 inner) (+ 1.0 outer))
        c (+ a (dr/random (- b a)))
        gap (dr/random 0.01 0.08)]
    (if (dr/chance 0.1)
      [(gl/line2 (tm/mix p q a) (tm/mix p q (- c gap)))
       (gl/line2 (tm/mix p q (+ c gap)) (tm/mix p q b))]
      [(gl/line2 (tm/mix p q a) (tm/mix p q b))])))

(defn reflect-hatching [{[x _] :p r :r :as c}]
  (-> c
      (clip/hatch-circle
       (tm/clamp (/ r 8.0) 3.0 8.0)
       (dr/gaussian 5.8 (* 0.2 (/ x (q/width)))))
      (remove-middle (inc (dr/random-int (int (/ r 6)))))))

(defn hatch-some-circles [circles]
  (dr/map-random-sample
   (fn [{[_ y] :p}] (eq/gaussian 0.05 (/ (q/height) 2) (/ (q/height) 8) y))
   (fn [c] (assoc c :hatching
                 (mapcat (partial stroke-line -0.03 0.09)
                         (reflect-hatching c))))
   circles))

;; note, can block until pack succeeds if radius is too high for current region
(defn radius-scale [n]
  (cond (<= n 6) 48.0
        (<= n 16.0) 32.0
        (<= n 24) 24.0
        (<= n 48) 12.0
        (<= n 64) 8.0
        :else 6.0))

(defn update-state [{:keys [bounds circles] :as state}]
  (let [n (count circles)
        radius (radius-scale n)
        pack-rules {:bounds bounds
                    :candidates 20
                    :gen-circle
                    (fn [] (gc/circle (rp/sample-point-inside bounds) radius))
                    :spacing (* radius 0.9)}]
    (if (>= n 160)
      state
      (-> state
          (update :circles pack/circle-pack pack-rules)
          (update :circles hatch-some-circles)))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/stroke-weight 0.7)
  (q/ellipse-mode :radius)
  (doseq [{:keys [hatching] :as c} circles]
    (cq/circle c)
    (when (seq hatching)
      (doseq [{[p q] :points} hatching]
        (q/line p q)))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition oil-reflections
    {:created-at "2021-09-05"
     :tags #{:static :deterministic}
     :type :quil}
  (ctrl/mount page))

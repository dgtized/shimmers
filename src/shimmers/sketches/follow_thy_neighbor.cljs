(ns shimmers.sketches.follow-thy-neighbor
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn bezier-line [points]
  (gl/linestrip2 (g/vertices (bezier/auto-spline2 points) 6)))

(defn make-line [left right displace]
  (bezier-line
   (for [t (tm/norm-range 10)]
     (tm/mix (g/point-at left t) (g/point-at right t) displace))))

(defn average-dist [a b samples]
  (/ (reduce + (map (fn [t] (g/dist (g/point-at a t) (g/point-at b t)))
                    (tm/norm-range samples)))
     (inc samples)))

(defn subdivide [lines]
  (let [idx (->> lines
                 (partition 2 1)
                 (map-indexed
                  (fn [idx [a b]]
                    (let [dist (average-dist a b 3)]
                      [idx (if (< dist 10) (* dist 0.2) dist)])))
                 dr/weighted)
        [before after] (split-at (inc idx) lines)
        a (last before)
        b (first after)
        t (dr/random 0.25 0.75)
        line (make-line a b t)]
    (if (or (collide/overlaps? a line)
            (collide/overlaps? b line))
      lines
      (concat before [line] after))))

(defn control-offsets [dist]
  (->> dist
       dr/weighted
       tm/norm-range
       cs/midsection))

(defn gen-lines []
  (let [d 0.03
        init
        (concat [(gl/line2 (rv -0.01 0.0) (rv -0.01 1.0))]
                (for [t (control-offsets {2 1
                                          3 4
                                          4 3
                                          5 1})]
                  (-> (concat [(rv (dr/gaussian t d) 0.0)]
                              (for [v (control-offsets {2 1
                                                        3 4
                                                        4 2})]
                                (rv (dr/gaussian t d) (dr/gaussian v (* d 0.66))))
                              [(rv (dr/gaussian t d) 1.0)])
                      bezier-line
                      (vary-meta assoc :stroke-width 3.0)))
                [(gl/line2 (rv 1.01 0.0) (rv 1.01 1.0))])]
    (last (take 30 (iterate subdivide init)))))

(defn cut-lines [lines]
  (dr/map-random-sample
   (constantly 0.2)
   (fn [line]
     (let [cuts (first (filter (fn [[a b]]  (> (- b a) 0.33))
                               (repeatedly #(sort [(dr/random 0.0 1.0) (dr/random 0.0 1.0)]))))]
       (with-meta line (assoc (meta line) :cuts cuts))))
   lines))

(defn splice [lines]
  (map (fn [line]
         (if-let [cuts (:cuts (meta line))]
           (let [points (lines/points-between (g/vertices line) (first cuts) (second cuts))]
             (if (seq points)
               (with-meta (gl/linestrip2 points) (meta line))
               line))
           line))
       lines))

(defn tick [line t r]
  (let [p (g/point-at line t)
        p1 (g/point-at line (- t 0.001))
        slope (tm/normalize (g/rotate (tm/- p p1) tm/HALF_PI) r)]
    (gl/line2 (tm/- p slope) (tm/+ p slope))))

(defn orb [line t r]
  (let [p (g/point-at line t)]
    (gc/circle p r)))

(defn arrow-down [line t r]
  (let [p (g/point-at line t)
        p1 (g/point-at line (- t 0.001))]
    (triangle/inscribed-equilateral
     (gc/circle p r) (g/heading (tm/- p p1)))))

(defn arrow-up [line t r]
  (let [p (g/point-at line t)
        p1 (g/point-at line (- t 0.001))]
    (triangle/inscribed-equilateral
     (gc/circle p r) (g/heading (tm/- p1 p)))))

(defn arrow-spin [line t r theta]
  (let [p (g/point-at line t)]
    (triangle/inscribed-equilateral
     (gc/circle p r) theta)))

(defn details [lines]
  (mapcat (fn [line]
            (let [decorate (dr/weighted [[tick 2] [orb 2] [arrow-up 1] [arrow-down 1] [arrow-spin 1]])
                  [lower upper] (or (:cuts (meta line))
                                    [0.0 1.0])]
              (for [t (dr/gaussian-range (dr/random 0.02 0.05) 0.002)
                    :when (<= lower t upper)]
                (decorate line t (dr/gaussian 3.0 0.25) (* t 0.33 tm/PHI eq/TAU)))))
          (take 11 (dr/shuffle (remove (fn [l] (:stroke-width (meta l))) lines)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (let [lines (cut-lines (gen-lines))]
      (concat
       (splice lines)
       (details lines)))))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :follow-thy-neighbor]
     [:div.readable-width]]))

(sketch/definition follow-thy-neighbor
  {:created-at "2023-11-30"
   :tags #{}
   :type :svg}
  (ctrl/mount page))

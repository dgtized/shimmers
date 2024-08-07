(ns shimmers.sketches.concentric-chords
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.bounded-shapes :as bounded]
   [shimmers.math.geometry.intersection :as isec]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn pack-overlap-circles [bounds n]
  (let [legal-candidate
        (fn [circles]
          (let [p (gv/vec2 (dr/random 0.1 0.9) (dr/random 0.1 0.9))
                candidate (g/scale-size (bounded/max-circle bounds p) 0.66)]
            (when-not (some
                       (fn [c] (> (geometry/percent-circle-overlap c candidate) 0.03))
                       circles)
              (conj circles candidate))))]
    (reduce (fn [circles _]
              (or (cs/retry 30 (partial legal-candidate circles))
                  circles))
            []
            (range n))))

(defn gen-chords [circle]
  (let [c (g/scale-size circle (dr/random 0.5 0.8))
        r-diff (- (:r circle) (:r c))
        inner (g/translate c (dr/jitter (dr/random r-diff)))]
    (->> (fn []
           (cs/retry 20
                     (fn []
                       (let [t (dr/random)
                             p (g/point-at inner t)
                             q (g/point-at circle
                                           (+ t (* (dr/rand-nth [1 1 1 -1])
                                                   (dr/random 0.08 0.2))))
                             line (gl/line2 p q)]
                         (when-not (isec/circle-line-intersect? inner line)
                           line)))))
         (repeatedly (* 25 (/ (:r circle) 50)))
         (into [inner] ))))

(defn shapes [bounds]
  (let [circles (pack-overlap-circles bounds 11)
        chords (mapcat gen-chords circles)]
    (concat circles chords)))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (g/scale-size (csvg/screen width height) 0.98))))

(sketch/definition concentric-chords
  {:created-at "2023-03-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (usvg/page sketch-args scene)))

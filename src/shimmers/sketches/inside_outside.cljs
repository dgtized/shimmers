(ns shimmers.sketches.inside-outside
  (:require
   [shimmers.algorithm.circle-packing :as pack]
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
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn generate-circles [bounds R]
  (reduce
   (fn [circles pct]
     (let [radius (* R pct)]
       (pack/circle-pack
        circles
        {:bounds bounds
         :candidates (min (int (/ 20 pct)) 800)
         :gen-circle
         (let [r (dr/random-int (* 0.75 radius) (* 1.25 radius))]
           (fn [] (gc/circle (g/random-point-inside bounds) r)))
         :spacing (max (* 0.005 R) (* 0.1 radius))})))
   []
   [0.2 0.12 0.1 0.08 0.06 0.04 0.02 0.01]))

(defn spiral [circle dr dt min-r]
  (->> {:circle circle :t (dr/random-tau) :r (:r circle)}
       (iterate
        (fn [{:keys [circle t r]}]
          (let [r' (* dr r)
                radial-pt (g/point-at circle (/ t eq/TAU))]
            {:circle (gc/circle (v/+polar radial-pt r' (- t Math/PI)) r')
             :t (+ t dt)
             :r r'})))
       (take-while (fn [{:keys [r]}] (> r min-r)))
       (map :circle)))

(defn concentric [circle dr min-r]
  (->> circle
       (iterate
        (fn [{:keys [p r]}]
          (gc/circle p (dr r))))
       (take-while (fn [{:keys [r]}] (> r min-r)))))

(defn restyle [circle]
  (let [min-r (* 0.01 (:r circle))]
    (case (dr/weighted {:spiral 5
                        :concentric-limit 1
                        :concentric-fixed 2})
      :spiral
      (spiral circle
              (dr/random 0.88 0.96)
              (* (dr/rand-nth [-1 1])
                 eq/TAU
                 (if (dr/chance 0.75)
                   (dr/random 0.05 0.2)
                   (dr/random 0.25 0.5)))
              min-r)
      :concentric-limit
      (concentric circle (let [dr (dr/random 0.875 0.94)]
                           (fn [r] (* r dr)))
                  min-r)
      :concentric-fixed
      (concentric circle (let [dr (* (:r circle) (dr/random 0.02 0.15))]
                           (fn [r] (- r dr)))
                  min-r))))

(defn shapes []
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.98)
        R (min (g/width bounds) (g/height bounds))
        circles (sort-by :r > (generate-circles bounds R))]
    (into [circles]
          (mapcat restyle (take 29 (dr/shuffle (take 45 circles)))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.33}
    (shapes)))

(sketch/definition inside-outside
    {:created-at "2023-11-17"
     :tags #{}
     :type :svg}
  (ctrl/mount (view-sketch/static-page scene :inside-outside)))

(ns shimmers.sketches.fibonacci-subdivisions
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn fib
  ([] (fib 1 1))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(comment (apply + (take 8 (fib))))

(defn axis-vector [{[x y] :size} axis]
  (if (= :x axis)
    (gv/vec2 x 0)
    (gv/vec2 0 y)))

(defn face-subdivide [shape cuts]
  (let [parcels (take cuts (fib))
        divisions (apply + parcels)
        [p q] (dr/rand-nth (g/edges shape))
        angle (g/normal (tm/- q p))
        c (g/centroid shape)
        shape-r (g/rotate (g/center shape c) (- (g/heading angle)))
        offset (g/rotate (axis-vector (g/bounds shape-r) :x) (g/heading angle))
        base (g/scale-size (gl/line2 p q) 100)
        lines
        (for [parcel parcels]
          (g/translate base (tm/* offset (/ parcel divisions))))]
    (reduce (fn [shapes cut]
              (mapcat (fn [shape]
                        (lines/cut-polygon shape cut))
                      shapes))
            [shape]
            lines)))

(defn subdivide [shape axis direction cuts]
  (let [parcels (take cuts (fib))
        divisions (apply + parcels)
        bounds (g/bounds shape)
        a (:p bounds)
        b (tm/+ a (axis-vector bounds ({:x :y :y :x} axis)))
        o (axis-vector bounds axis)
        base (if (= direction :asc) (gv/vec2) o)
        offset (if (= direction :asc) o (tm/- o))
        lines
        (for [parcel parcels]
          (gl/line2 (tm/+ (tm/+ base a) (tm/* offset (/ parcel divisions)))
                    (tm/+ (tm/+ base b) (tm/* offset (/ parcel divisions)))))]
    (reduce (fn [shapes cut]
              (mapcat (fn [shape]
                        (lines/cut-polygon shape cut))
                      shapes))
            [shape]
            lines)))

(defn shapes [bounds palette]
  (loop [depth 21 shapes [bounds]]
    (if (zero? depth)
      (let [weighted-palette (zipmap (dr/shuffle (into ["none"] palette))
                                     (take (+ 1 (count palette)) (fib)))]
        (map (fn [s]
               ;; FIXME: more offset bugs from inset tossing shapes around
               (vary-meta (poly-detect/inset-polygon s 2)
                          assoc :fill (dr/weighted weighted-palette)))
             shapes))
      (recur (dec depth)
             (dr/mapcat-random-sample
              (fn [s] (/ (g/area s) (+ (g/area bounds) 1)))
              (fn [s]
                (let [{[w h] :size} (g/bounds s)
                      n (dr/weighted (let [s (take (dr/rand-nth (range 5 8)) (fib))]
                                       (zipmap (drop 2 s) (reverse s))))]
                  (if (dr/chance 0.25)
                    (subdivide s
                               (if (> w h) :x :y)
                               (dr/weighted {:asc 1 :desc 1})
                               n)
                    (face-subdivide s n))))
              shapes)))))

(defn scene [{:keys [scene-id palette]}]
  (fn []
    (csvg/svg-timed {:id scene-id
                     :width width
                     :height height
                     :stroke "black"
                     :fill "none"
                     :stroke-width 0.5}
      (shapes (if (dr/chance 0.5)
                (g/center (poly/regular-n-gon (dr/random-int 3 9)
                                              (* 0.5 height))
                          (rv 0.5 0.5))
                (g/scale-size (csvg/screen width height) 0.99))
              palette))))

(defn explanation [{:keys [palette]}]
  [:div.evencols
   [:div.readable-width
    [:p "Genuary 2025 - Day 12 - Subdivisions"]
    [:p "Subdivision ratios and probabilities are all derived from fibonnanci
    sequence weighting. Even the palette distribution is weighted by fibonnanci
    values."]]
   [:p
    [palette/as-svg {} palette]]])

(sketch/definition fibonacci-subdivisions
  {:created-at "2025-01-12"
   :tags #{:genuary2025}
   :type :svg}
  (ctrl/mount
   (let [palette (:colors (dr/rand-nth palette/db))]
     (usvg/page (assoc sketch-args :palette palette
                       :explanation explanation) scene))))

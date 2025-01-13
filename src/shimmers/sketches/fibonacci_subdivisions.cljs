(ns shimmers.sketches.fibonacci-subdivisions
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
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
  (loop [depth 8 shapes [bounds]]
    (if (zero? depth)
      (let [weighted-palette (zipmap (dr/shuffle (into ["none"] palette))
                                     (take (+ 1 (count palette)) (fib)))]
        (map (fn [s]
               (vary-meta (g/scale-size s 0.99)
                          assoc :fill (dr/weighted weighted-palette)))
             shapes))
      (recur (dec depth)
             (dr/mapcat-random-sample
              (fn [s] (/ (g/area s) (+ (g/area bounds) 1)))
              (fn [s]
                (let [{[w h] :size} (g/bounds s)]
                  (subdivide s
                             (if (> w h) :x :y)
                             (dr/weighted {:asc 1 :desc 1})
                             (dr/weighted (let [s (take (dr/rand-nth (range 5 8)) (fib))]
                                            (zipmap (drop 2 s) (reverse s)))))))
              shapes)))))

(defn scene [{:keys [scene-id palette]}]
  (fn []
    (csvg/svg-timed {:id scene-id
                     :width width
                     :height height
                     :stroke "black"
                     :fill "none"
                     :stroke-width 0.5}
      (shapes (g/scale-size (csvg/screen width height) 0.99)
              palette))))

(defn explanation [{:keys [palette]}]
  [:div.evencols
   [:div.readable-width
    [:p "Genuary 2025 - Day 12 - Subdivisions"]]
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

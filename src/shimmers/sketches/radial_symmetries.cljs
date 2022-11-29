(ns shimmers.sketches.radial-symmetries
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn individual [idx {:keys [ring coord] :as hex}]
  (let [poly (hex/flat-hexagon->polygon hex)]
    (csvg/group {}
      (vary-meta poly assoc :fill (csvg/hsl (* ring tm/PHI) 0.8 0.8 1.0))
      (csvg/center-label (:p hex)
                         (str idx "\n" coord)
                         {:font-size "0.5em"}))))

(defn same-or-adjacent? [n-vertices idx-a idx-b]
  (or (= idx-a idx-b)
      (= idx-b (mod (inc idx-a) n-vertices))
      (= idx-b (mod (dec idx-a) n-vertices))))

(defn random-cut [poly i]
  (let [vertices (g/vertices poly)
        idx-a (dr/random-int (count vertices))
        idx-b (dr/random-int (count vertices))]
    (if (same-or-adjacent? (count vertices) idx-a idx-b)
      (recur poly i)
      (let [cut-line (gl/line2 (nth vertices idx-a)
                               (nth vertices idx-b))]
        (conj (lines/cut-polygon poly cut-line)
              (vary-meta cut-line assoc :stroke-width 2.0))))))

(defn seq-cut [poly base dist]
  (let [vertices (g/vertices poly)
        idx-a base
        idx-b (+ base dist)
        n-vertices (count vertices)]
    (cond
      (> base dist) poly
      (same-or-adjacent? n-vertices idx-a idx-b)
      (recur poly base (inc dist))
      :else
      (let [cut-line (gl/line2 (nth vertices (mod idx-a n-vertices))
                               (nth vertices (mod idx-b n-vertices)))]
        (conj (lines/cut-polygon poly cut-line)
              #_(vary-meta cut-line assoc :stroke-width 1))))))

(defn inset-circle [poly _i]
  (let [p (g/centroid poly)
        [a b] (first (g/edges poly))]
    (gc/circle p (g/dist p (tm/mix a b 0.5)))))

(defn inset-rectangle [poly i]
  (let [[a b _ d e _] (cs/rotate i (g/vertices poly))]
    (gp/polygon2 a b d e)))

(defn inset-triangles [poly i]
  (let [c (g/centroid poly)]
    (map (fn [[a b]] (gt/triangle2 a b c))
         (g/edges poly))))

(defn inset-pointy [poly _i]
  (-> poly
      (geometry/rotate-around-centroid (/ eq/TAU 12))
      (g/scale-size (/ tm/SQRT3 2))))

(defn coord-label [poly idx {:keys [p coord]}]
  (csvg/group {}
    (vary-meta poly assoc :fill (csvg/hsl (* idx tm/PHI) 0.8 0.8 1.0))
    (csvg/center-label p
                       (str idx "\n" coord)
                       {:font-size "0.5em"})))

(defn on-zeros [rule]
  (fn [poly i]
    (if (zero? i)
      (rule poly i)
      poly)))

(def shape-rules [identity inset-pointy inset-circle inset-triangles inset-rectangle])

(defn pair-rythm [rule-a rule-b freq]
  (let [half-freq (int (/ freq 2))]
    (fn [poly i]
      (cond (zero? i)
            (rule-a poly i)
            (= i half-freq)
            (rule-b poly i)
            :else poly))))

(defn polyrythm [freq]
  (let [patterns (repeatedly freq #(dr/rand-nth shape-rules))]
    (fn [poly i]
      ((nth patterns i) poly i))))

(defn deeper [rule freq]
  (let [dir (dr/rand-nth [(fn [i] (- freq i))
                          inc])]
    (fn [poly i]
      (let [polygon (rule poly i)]
        (csvg/group {}
          (for [s (take (dir i) (range 0.0 0.8 (/ 1.0 freq)))]
            (g/scale-size polygon (- 1.0 s))))))))

(defn change-hex [operator freq idx hex]
  (let [i (mod idx freq)
        poly (hex/flat-hexagon->polygon hex)]
    (csvg/group {}
      (operator poly i))))

(defn hexagons [revolutions]
  (let [radius (* 0.95 height)
        hex-radius (/ radius (* 3 (+ revolutions 2.5)))
        hexes (mapv (fn [hex]
                      (assoc (hex/cube-hexagon hex hex-radius)
                             :coord hex
                             :ring (hex/cube-distance (gv/vec3) hex)))
                    (hex/cube-spiral (gv/vec3) revolutions))]
    #_(map-indexed individual hexes)
    (->> (partition-by :ring hexes)
         (mapcat (fn [ring]
                   (let [n (count ring)
                         freq (if (= n 1)
                                1
                                (dr/rand-nth (butlast (sm/factors n 11))))
                         rule (if (= n 1)
                                identity
                                (dr/weighted {inset-rectangle 1
                                              identity 1
                                              (on-zeros inset-triangles) 1
                                              (on-zeros inset-circle) 1
                                              (on-zeros inset-pointy) 1
                                              (deeper inset-circle freq) (if (<= freq 8) 1 0)
                                              (deeper inset-pointy freq) (if (<= freq 8) 1 0)
                                              (deeper identity freq) (if (<= freq 8) 1 0)
                                              (pair-rythm inset-circle inset-pointy freq) (if (> freq 1) 1 0)
                                              (pair-rythm inset-circle inset-rectangle freq) (if (> freq 1) 1 0)
                                              (pair-rythm inset-pointy inset-rectangle freq) (if (> freq 1) 1 0)
                                              (polyrythm freq) 2
                                              (fn [p i] (seq-cut p i freq)) 4}))]
                     (map-indexed (partial change-hex rule freq) ring)))))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 0.66}
     (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
       (hexagons 17)))))

(sketch/definition radial-symmetries
  {:created-at "2022-11-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :radial-symmetries)
              "sketch-host"))

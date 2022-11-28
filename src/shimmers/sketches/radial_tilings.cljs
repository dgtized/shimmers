(ns shimmers.sketches.radial-tilings
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
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
    (if (same-or-adjacent? n-vertices idx-a idx-b)
      (recur poly base (inc dist))
      (let [cut-line (gl/line2 (nth vertices (mod idx-a n-vertices))
                               (nth vertices (mod idx-b n-vertices)))]
        (conj (lines/cut-polygon poly cut-line)
              (vary-meta cut-line assoc :stroke-width 2.0))))))

(defn inset [poly _i]
  (let [p (g/centroid poly)
        [a b] (first (g/edges poly))]
    (gc/circle p (g/dist p (tm/mix a b 0.5)))))

(defn coord-label [poly idx {:keys [p coord]}]
  (csvg/group {}
    (vary-meta poly assoc :fill (csvg/hsl (* idx tm/PHI) 0.8 0.8 1.0))
    (csvg/center-label p
                       (str idx "\n" coord)
                       {:font-size "0.5em"})))

(defn slice-hex [operator freq idx hex]
  (let [i (mod idx freq)
        poly (hex/flat-hexagon->polygon hex)]
    (if (zero? i)
      (csvg/group {}
        (operator poly i))
      poly)))

(defn hexagons []
  (let [radius (* 0.95 height)
        revolutions 12
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
                                (dr/rand-nth (sm/factors n 12)))
                         rule (dr/rand-nth [inset (fn [p i] (seq-cut p i (dr/random-int 2 5)))])]
                     (map-indexed (partial slice-hex rule freq) ring)))))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 0.5}
     (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
       (hexagons)))))

(sketch/definition radial-tilings
  {:created-at "2022-11-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :radial-tilings)
              "sketch-host"))

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

(defn cut [poly]
  (let [vertices (g/vertices poly)
        idx-a (dr/random-int (count vertices))
        idx-b (dr/random-int (count vertices))]
    (if (and (not= idx-a idx-b)
             (not (or (= idx-b (mod (inc idx-a) (count vertices)))
                      (= idx-b (mod (dec idx-a) (count vertices))))))
      (lines/cut-polygon poly
                         (gl/line2 (nth vertices idx-a)
                                   (nth vertices idx-b)))
      (recur poly))))

(defn slice-hex [operator freq idx {:keys [ring coord] :as hex}]
  (let [i (mod idx freq)
        poly (hex/flat-hexagon->polygon hex)]
    (if true
      (csvg/group {}
        (operator poly))
      (csvg/group {}
        (vary-meta poly assoc :fill (csvg/hsl (* i tm/PHI) 0.8 0.8 1.0))
        (csvg/center-label (:p hex)
                           (str idx "\n" coord)
                           {:font-size "0.5em"})))))

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
                         rule (dr/rand-nth [cut])]
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

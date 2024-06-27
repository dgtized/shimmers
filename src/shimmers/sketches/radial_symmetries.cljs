(ns shimmers.sketches.radial-symmetries
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.svg-export :as svg-export]
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

(defn inset-triangles [poly _i]
  (let [c (g/centroid poly)]
    (map (fn [[a b]] (gt/triangle2 a b c))
         (g/edges poly))))

(defn deeper-triangles [scale]
  (fn triangles [poly idx]
    (mapcat (fn [s] [s (g/scale-size s scale)])
            (inset-triangles poly idx))))

(defn inset-pointy [poly _i]
  (-> poly
      (geometry/rotate-around-centroid (/ eq/TAU 12))
      (g/scale-size eq/SQRT3_2)))

(defn coord-label [poly idx {:keys [p coord]}]
  (csvg/group {}
    (vary-meta poly assoc :fill (csvg/hsl (* idx tm/PHI) 0.8 0.8 1.0))
    (csvg/center-label p
                       (str idx "\n" coord)
                       {:font-size "0.5em"})))

(def shape-rules
  [identity
   inset-pointy
   inset-circle
   inset-triangles
   inset-rectangle
   (deeper-triangles 0.4)
   (deeper-triangles 0.5)])

(defn on-zeros [rule freq]
  (fn on-zero [poly idx]
    (let [i (mod idx freq)]
      (if (zero? i)
        (rule poly i)
        poly))))

(defn pair-rythm [rule-a rule-b freq]
  (let [half-freq (int (/ freq 2))]
    (fn pair-rythm [poly idx]
      (let [i (mod idx freq)]
        (cond (zero? i)
              (rule-a poly i)
              (= i half-freq)
              (rule-b poly i)
              :else poly)))))

(defn polyrythm [pattern freq]
  (fn [poly idx]
    (let [i (mod idx freq)]
      ((nth pattern i) poly i))))

(defn deeper [rule dir freq]
  (fn deeper [poly idx]
    (let [i (mod idx freq)
          polygon (rule poly i)]
      (csvg/group {}
        (for [s (take (dir i) (range 0.0 0.8 (/ 1.0 freq)))]
          (g/scale-size polygon (- 1.0 s)))))))

(defn sequence-cut [freq]
  (fn seq-cut' [poly idx]
    (seq-cut poly idx freq)))

;; FIXME: not deterministic from initial seed except at reload?
(defn generate-rule [n]
  (if (= n 1)
    identity
    (let [factors (sm/factors n 11)
          freq (if (empty? factors) 1 (dr/rand-nth factors))
          dir (dr/rand-nth [(fn [i] (- freq i)) inc])]
      (dr/weighted
       [[inset-rectangle 1]
        [identity 1]
        [(on-zeros inset-triangles freq) 1]
        [(on-zeros inset-circle freq) 1]
        [(on-zeros inset-pointy freq) 1]
        [(on-zeros (deeper-triangles 0.4) freq) 1]
        [(on-zeros (deeper-triangles 0.5) freq) 1]
        [(deeper inset-circle dir freq) (if (<= freq 8) 1 0)]
        [(deeper inset-pointy dir freq) (if (<= freq 8) 1 0)]
        [(deeper inset-rectangle dir freq) (if (<= freq 8) 1 0)]
        [(deeper identity dir freq) (if (<= freq 8) 1 0)]
        [(pair-rythm inset-circle inset-pointy freq) 1]
        [(pair-rythm inset-circle inset-rectangle freq) 1]
        [(pair-rythm inset-pointy inset-rectangle freq) 1]
        [(polyrythm (repeatedly freq #(dr/rand-nth shape-rules)) freq) 2]
        [(sequence-cut freq) 4]]))))

(defn change-hexes [ring]
  (let [rule (generate-rule (count ring))]
    (map-indexed (fn change-hex [idx hex]
                   (let [poly (hex/flat-hexagon->polygon hex)]
                     (csvg/group {} (rule poly idx))))
                 ring)))

(defn hexagons [revolutions]
  (let [radius (* 0.95 height)
        hex-radius (/ radius (* 3 (+ revolutions 2.5)))]
    (->> (hex/cube-spiral (gv/vec3) revolutions)
         (mapv (fn [hex]
                 (assoc (hex/cube-hexagon hex hex-radius)
                        :coord hex
                        :ring (hex/cube-distance (gv/vec3) hex))))
         (partition-by :ring)
         (mapcat change-hexes))))

(defn scene []
  (csvg/svg-timed {:id "scene"
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.66}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (hexagons 17))))

(defn ui-controls []
  [kb/kb-action "alt-s" #(svg-export/download "scene" "radial-symmetries")])

(sketch/definition radial-symmetries
  {:created-at "2022-11-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :radial-symmetries ui-controls)))

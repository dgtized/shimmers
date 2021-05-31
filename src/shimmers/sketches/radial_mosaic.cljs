(ns shimmers.sketches.radial-mosaic
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.color :as color]
            [shimmers.math.deterministic-random :as dr]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.strf.core :as f]))

(def width 900)
(def height 600)
(defn r [x y]
  (gv/vec2 (* x width) (* y height)))

(defn polar [r theta]
  (geom/as-cartesian (gv/vec2 r theta)))

(defn radial-range [n st]
  (let [r (range st (+ tm/TWO_PI st) (/ tm/TWO_PI n))
        segments (vec (map vec (partition 2 1 r)))]
    (if (tm/delta= (last r) (+ tm/TWO_PI st))
      segments
      (conj segments [(last r) (first r)]))))

(comment (radial-range 14 0.05))

(defn first-last [coll]
  [(first coll) (last coll)])

(defn partition-segments [chunks pads coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [n (first chunks)
           p (take n s)]
       (if (== n (count p))
         (cons (first-last p)
               (partition-segments (rest chunks) (rest pads)
                                   (drop (+ n (first pads)) s)))
         (list (first-last (take n p))))))))

(comment (partition-segments #(int (tm/random 4 24))
                             #(int (tm/random 0 4))
                             (range 100))
         (partition-segments (cycle [4 8 16])
                             (cycle [1 2])
                             (range 100)))

(defn factors [n k]
  (filter (fn [factor] (= (mod n factor) 0)) (range 2 k)))

(comment (map (fn [n] [n (factors n 9)]) (range 1 100)))

(defn segment [t0 t1 r0 r1 attribs]
  (let [lower (polar r0 t0)
        upper (polar r1 t1)]
    (svg/path [[:M lower]
               [:L (polar r1 t0)]
               [:A [r1 r1] 0.0 0 1 upper]
               [:L (polar r0 t1)]
               [:A [r0 r0] 0.0 0 0 lower]
               [:Z]]
              (merge
               {:fill "none"
                :stroke-width 0.6
                :stroke "black"
                :key (str "s:" t0 "-" t1 "-" r0)}
               attribs))))

(comment
  (f/format (:A svg/path-segment-formats) (gv/vec2 0.5 0.1) 1.0 1.0 1.0 (gv/vec2 1.0 0.5))
  (f/format [(f/float 2)] 0.21)
  (segment 0.5 1 1 2 {}))

(defn svg-translate [p]
  (apply f/format ["translate(" (f/float 2) "," (f/float 2) ")"]
         p))

;; First palette is more in pastel range, seems like that fits this better?
;; Maybe just because it's also ensuring "none" is used a lot?
(def palettes
  (->> ["https://artsexperiments.withgoogle.com/artpalette/colors/c8cccc-7c9aa8-ede4da-a5b6c0-e0c1a2"
        "https://artsexperiments.withgoogle.com/artpalette/colors/51467c-dccfbe-d4ba90-aa8c60-726665"
        "https://artsexperiments.withgoogle.com/artpalette/colors/e7eef0-759acd-81a4d1-9f9a98-454d7d"
        "https://artsexperiments.withgoogle.com/artpalette/colors/d4ddda-51988e-274b75-a0b5c0-2d5429"
        "https://artsexperiments.withgoogle.com/artpalette/colors/2f403d-e9e6d9-b4533a-9b9270-ddbd67"]
       (map color/url->colors)
       (map (partial map (partial str "#")))))

(defn palette-sequence [palette segments]
  (let [multiple (let [m (factors segments 10)]
                   (if (empty? m)
                     1
                     (dr/drand-nth m)))
        colors (into palette ["none" "none"])]
    (repeatedly multiple #(dr/drand-nth colors))))

(comment (palette-sequence (first palettes) 19))

;; Cycle through segment theta rotations? ie 2,4,8 radial arcs?
;; Consider adding a "dispersion" line that displaces all tiles it touches outwards slightly?
;; Alternatively, consider giving it more "wear" by ensuring radial spacing and
;; rotating every shape slightly around it's own centroid?
(defn scene [{:keys [origin palette radius]}]
  (->> (map vector
            (partition-segments (cycle [5 13 8 21 5 8 13])
                                (cycle [1 1 2])
                                radius)
            (repeatedly #(dr/drand-int 16 24))
            (repeatedly #(dr/drandom 0.0 0.05)))
       (mapcat (fn [[[r0 r1] n st]]
                 (let [segments (* n (inc (int (/ r1 50))))
                       row-palette (palette-sequence palette segments)
                       spacing (/ 1.5 r1)]
                   (for [[[t0 t1] color]
                         (map vector
                              (radial-range segments st)
                              (cycle row-palette))]
                     (segment (+ t0 spacing) t1 r0 r1 {:fill color})))))
       (svg/group {:transform (svg-translate origin)}
                  (with-meta (gc/circle (gv/vec2) (first radius))
                    {:fill (dr/drand-nth palette)}))
       (csvg/svg {:width width :height height})))

(defn scenes []
  (->> [{:origin (r (dr/drand-nth [0.4 0.5 0.6]) 0.5)
         :radius (range 6 (int (* 0.5 height)))}
        {:origin (r (dr/drand-nth [0.33 0.66]) 0.5)
         :radius (range 6 (int (* 0.6 width)))}
        {:origin (r (dr/drand-nth [0.2 0.3 0.7 0.8]) (dr/drand-nth [0.33 0.4 0.6 0.66]) )
         :radius (range 6 (int (* (dr/drand-nth [0.6 0.7 0.8 0.9]) width)))}]
       dr/drand-nth
       (merge {:palette (dr/drand-nth palettes)})
       scene))

(defn page []
  [:div (scenes)])

(defn ^:export run-sketch []
  ;; 20210515
  (ctrl/mount page "svg-host"))

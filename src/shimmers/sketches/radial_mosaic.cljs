(ns shimmers.sketches.radial-mosaic
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.color :as color]
            [shimmers.math.probability :as p]
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

(defn radial-range [dt]
  (let [r (range 0 tm/TWO_PI dt)]
    (conj (vec (map vec (partition 2 1 r))) [(last r) (first r)])))

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

(defn segment [t0 t1 r0 r1 attribs]
  (let [[x0 y0] (polar r0 t0)
        [x1 y1] (polar r1 t1)]
    (svg/path [[:M (polar r0 t0)]
               [:L (polar r1 t0)]
               #_[:L (polar r1 t1)]
               [:A [r1 r1] 0.0 0 1 [x1 y1]]
               [:L (polar r0 t1)]
               #_[:L (polar r0 t0)]
               [:A [r0 r0] 0.0 0 0 [x0 y0]]
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
        "https://artsexperiments.withgoogle.com/artpalette/colors/e7eef0-759acd-81a4d1-9f9a98-454d7d"]
       (map color/url->colors)
       (map (partial map (partial str "#")))))

(defn palette-sequence [palette]
  (let [colors (assoc (zipmap palette (repeat 1))
                      "none" 4)]
    (take (int (tm/random 3 6)) (repeatedly #(p/weighted colors)))))

(comment (palette-sequence (first palettes)))

;; Add grout padding between radial segments?
;; Cycle through segment theta rotations? ie 2,4,8 radial arcs?
(defn scene [origin palette radius]
  (->> (map vector
            (partition-segments (cycle [5 13 8 21 5 8 13])
                                (cycle [1 1 2])
                                radius)
            (repeatedly #(int (tm/random 16 24)))
            (repeatedly #(tm/random 0.0 0.2)))
       (mapcat (fn [[[r0 r1] segments st]]
                 (let [dt (/ tm/TWO_PI (* segments (inc (int (/ r1 50)))))]
                   (for [[[t0 t1] color]
                         (map vector (radial-range dt) (cycle (palette-sequence palette)))]
                     (segment (+ st t0) (+ st t1) r0 r1 {:fill color})))))
       (svg/group {:transform (svg-translate origin)}
                  (with-meta (gc/circle (gv/vec2) (first radius))
                    {:fill (rand-nth palette)}))
       (csvg/svg {:width width :height height})))

(defn page []
  [:div (scene (r 0.5 0.5) (rand-nth palettes)
               (range 6 (int (* 0.5 height))))])

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))

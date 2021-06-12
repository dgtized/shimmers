(ns shimmers.sketches.radial-mosaic
  (:require [shimmers.common.sequence :as cs]
            [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.color :as color]
            [shimmers.math.core :as sm]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.vector :as v]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.strf.core :as f]))

(def width 900)
(def height 600)
(defn r [x y]
  (gv/vec2 (* x width) (* y height)))

(defn radial-range [n st]
  (let [r (range st (+ tm/TWO_PI st) (/ tm/TWO_PI n))
        segments (vec (map vec (partition 2 1 r)))]
    (if (tm/delta= (last r) (+ tm/TWO_PI st))
      segments
      (conj segments [(last r) (first r)]))))

(comment (radial-range 14 0.05))

(defn factors [n k]
  (filter (fn [factor] (= (mod n factor) 0)) (range 2 k)))

(comment (map (fn [n] [n (factors n 9)]) (range 1 100)))

(defn draw-segment [t0 t1 r0 r1 attribs]
  (let [lower (v/polar r0 t0)
        upper (v/polar r1 t1)]
    (svg/path [[:M lower]
               [:L (v/polar r1 t0)]
               [:A [r1 r1] 0.0 0 1 upper]
               [:L (v/polar r0 t1)]
               [:A [r0 r0] 0.0 0 0 lower]
               [:Z]]
              (merge
               {:fill "white"
                :stroke-width 0.6
                :stroke "black"
                :key (str "s:" t0 "-" t1 "-" r0)}
               attribs))))

(comment
  (f/format (:A svg/path-segment-formats) (gv/vec2 0.5 0.1) 1.0 1.0 1.0 (gv/vec2 1.0 0.5))
  (f/format [(f/float 2)] 0.21)
  (draw-segment 0.5 1 1 2 {}))

(defn segment [t0 t1 r0 r1 attribs displacement]
  (let [{:keys [arc0 arc1 percent force]} displacement
        maybe-transformed
        (if (and (or (sm/radians-between? arc0 arc1 t0)
                     (sm/radians-between? arc0 arc1 t1))
                 false ;; disabled for now
                 (dr/chance percent))
          (let [center-r (/ (+ r0 r1) 2)
                center-theta (sm/mix-mod t0 t1 0.5 tm/TWO_PI)
                center (v/polar center-r center-theta)
                rotation (* 1.5 (- center-theta (/ (+ arc0 arc1) 2)))
                f (* force center-r)
                transforms [(csvg/translate (v/polar f center-theta))
                            (csvg/rotate rotation center)]]
            (merge attribs {:transform (apply str (interpose " " transforms))}))
          attribs)]
    (draw-segment t0 t1 r0 r1 maybe-transformed)))

;; First palette is more in pastel range, seems like that fits this better?
;; Maybe just because it's also ensuring "none" is used a lot?
(def palettes
  (->> ["https://artsexperiments.withgoogle.com/artpalette/colors/c8cccc-7c9aa8-ede4da-a5b6c0-e0c1a2"
        "https://artsexperiments.withgoogle.com/artpalette/colors/51467c-dccfbe-d4ba90-aa8c60-726665"
        "https://artsexperiments.withgoogle.com/artpalette/colors/e7eef0-759acd-81a4d1-9f9a98-454d7d"
        "https://artsexperiments.withgoogle.com/artpalette/colors/d4ddda-51988e-274b75-a0b5c0-2d5429"
        "https://artsexperiments.withgoogle.com/artpalette/colors/2f403d-e9e6d9-b4533a-9b9270-ddbd67"
        ;; some bolder palettes
        "https://artsexperiments.withgoogle.com/artpalette/colors/adc7e5-e1e6e7-5087ba-b89474-222982"
        "https://artsexperiments.withgoogle.com/artpalette/colors/c5962a-30497c-dddecf-7b7b75-8f3020"
        "https://artsexperiments.withgoogle.com/artpalette/colors/b1bfc5-212720-6f8f48-49583d-5081ad"
        "https://artsexperiments.withgoogle.com/artpalette/colors/ca2825-161519-d6c844-979593-0b5999"]
       (map color/url->colors)
       (map (partial map (partial str "#")))))

(defn palette-sequence [palette segments]
  (let [multiple (let [m (factors segments 10)]
                   (if (empty? m)
                     1
                     (dr/rand-nth m)))
        colors (into palette ["white" "white"])]
    (repeatedly multiple #(dr/rand-nth colors))))

(comment (palette-sequence (first palettes) 19))

;; Cycle through segment theta rotations? ie 2,4,8 radial arcs?
;; Consider adding a "dispersion" line that displaces all tiles it touches outwards slightly?
;; Alternatively, consider giving it more "wear" by ensuring radial spacing and
;; rotating every shape slightly around it's own centroid?
(defn scene [{:keys [origin palette radius displacement]}]
  (->> (map vector
            (cs/partition-segments (cycle [5 13 8 21 5 8 13])
                                   (cycle [1 1 2])
                                   radius)
            (repeatedly #(dr/random-int 16 24))
            (repeatedly #(dr/random 0.05)))
       (mapcat (fn [[[r0 r1] n st]]
                 (let [segments (* n (inc (int (/ r1 50))))
                       row-palette (palette-sequence palette segments)
                       spacing (/ 1.5 r1)]
                   (for [[[t0 t1] color]
                         (map vector
                              (radial-range segments st)
                              (cycle row-palette))]
                     (segment (+ t0 spacing) t1 r0 r1 {:fill color}
                              displacement)))))
       (svg/group {:transform (csvg/translate origin)}
                  (with-meta (gc/circle (gv/vec2) (first radius))
                    {:fill (dr/rand-nth palette)}))
       (csvg/svg {:width width :height height})))

(defn scenes []
  (->> [{:origin (r (dr/rand-nth [0.4 0.5 0.6]) 0.5)
         :radius (range 6 (int (* 0.5 height)))}
        {:origin (r (dr/rand-nth [0.33 0.66]) 0.5)
         :radius (range 6 (int (* 0.6 width)))}
        {:origin (r (dr/rand-nth [0.2 0.3 0.7 0.8]) (dr/rand-nth [0.33 0.4 0.6 0.66]) )
         :radius (range 6 (int (* (dr/rand-nth [0.6 0.7 0.8 0.9]) width)))}]
       dr/rand-nth
       (merge {:palette (dr/rand-nth palettes)
               ;; TODO: set arc0 to arc1 to be close to a far corner from
               ;; center? Also, Consider setting a single theta with a radial
               ;; width and displace more the closer the piece is to theta?
               :displacement {:arc0 5.95 :arc1 6.25 :percent 1.0 :force 0.3}})
       scene))

(defn page []
  [:div (scenes)])

(defn ^:export run-sketch []
  ;; 20210515
  (ctrl/mount page "svg-host"))

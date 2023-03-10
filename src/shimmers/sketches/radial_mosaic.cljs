(ns shimmers.sketches.radial-mosaic
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.palette :as palette]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def settings (ctrl/state {:dispersion false}))
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

(defn segment [t0 t1 r0 r1 attribs displacement]
  (let [{:keys [arc0 arc1 percent force]} displacement
        maybe-transformed
        (if (and (or (sm/radians-between? arc0 arc1 t0)
                     (sm/radians-between? arc0 arc1 t1))
                 (:dispersion @settings)
                 (dr/chance percent))
          (let [center-r (/ (+ r0 r1) 2)
                center-theta (sm/radial-mix t0 t1 0.5)
                center (v/polar center-r center-theta)
                rotation (* 1.0 (- center-theta (/ (+ arc0 arc1) 2)))
                f (* force center-r)
                transforms [(csvg/translate (v/polar f center-theta))
                            (csvg/rotate rotation center)]]
            (merge attribs {:transform (apply str (interpose " " transforms))}))
          attribs)]
    (csvg/arc-segment t0 t1 r0 r1
                      (merge
                       {:fill "white"
                        :stroke-width 0.6
                        :stroke "black"}
                       maybe-transformed))))

;; First palette is more in pastel range, seems like that fits this better?
;; Maybe just because it's also ensuring "none" is used a lot?
(def palettes
  (palette/from-urls
   ["https://artsexperiments.withgoogle.com/artpalette/colors/c8cccc-7c9aa8-ede4da-a5b6c0-e0c1a2"
    "https://artsexperiments.withgoogle.com/artpalette/colors/51467c-dccfbe-d4ba90-aa8c60-726665"
    "https://artsexperiments.withgoogle.com/artpalette/colors/e7eef0-759acd-81a4d1-9f9a98-454d7d"
    "https://artsexperiments.withgoogle.com/artpalette/colors/d4ddda-51988e-274b75-a0b5c0-2d5429"
    "https://artsexperiments.withgoogle.com/artpalette/colors/2f403d-e9e6d9-b4533a-9b9270-ddbd67"
    ;; some bolder palettes
    "https://artsexperiments.withgoogle.com/artpalette/colors/adc7e5-e1e6e7-5087ba-b89474-222982"
    "https://artsexperiments.withgoogle.com/artpalette/colors/c5962a-30497c-dddecf-7b7b75-8f3020"
    "https://artsexperiments.withgoogle.com/artpalette/colors/b1bfc5-212720-6f8f48-49583d-5081ad"
    "https://artsexperiments.withgoogle.com/artpalette/colors/ca2825-161519-d6c844-979593-0b5999"
    ;; orange blue red
    "https://artsexperiments.withgoogle.com/artpalette/colors/db9003-332f2e-20778c-d8cdb9-ba3a29"
    ;; orange maroon blues
    "https://artsexperiments.withgoogle.com/artpalette/colors/0c3c56-236884-ce5110-3e160e-338bab"
    "https://artsexperiments.withgoogle.com/artpalette/colors/204354-34a3bb-f34c1c-241f1e-c0bbb8"]))

(defn palette-sequence [palette segments]
  (let [multiple (let [m (sm/factors segments 10)]
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
       (csvg/group {:transform (csvg/translate origin)}
         (with-meta (gc/circle (gv/vec2) (first radius))
           {:fill (dr/rand-nth palette)}))))

(defn mosaic-params []
  (->> [{:origin (r (dr/rand-nth [0.4 0.5 0.6]) 0.5)
         :radius (range 6 (int (* 0.5 height)))}
        {:origin (r (dr/rand-nth [0.33 0.66]) 0.5)
         :radius (range 6 (int (* 0.6 width)))}
        {:origin (r (dr/rand-nth [0.2 0.3 0.7 0.8]) (dr/rand-nth [0.33 0.4 0.6 0.66]))
         :radius (range 6 (int (* (dr/rand-nth [0.6 0.7 0.8 0.9]) width)))}]
       dr/rand-nth
       (merge {:palette (dr/rand-nth palettes)
               ;; TODO: set arc0 to arc1 to be close to a far corner from
               ;; center? Also, Consider setting a single theta with a radial
               ;; width and displace more the closer the piece is to theta?
               :displacement {:arc0 -0.5 :arc1 0.5 :percent 1.0 :force 0.3}})))

(defn page []
  (let [{:keys [palette] :as params} (mosaic-params)]
    (fn []
      [:div
       [:div.canvas-frame
        (csvg/svg-timed {:id "scene"
                         :width width
                         :height height}
          [(scene params)])
        [kb/kb-action "alt-s" #(svg-export/download "scene" "radial-mosaic")]]
       [:p.center (view-sketch/generate :radial-mosaic)]
       [palette/as-svg {:class "center"
                        :width (* 40 (count palette))
                        :height 30}
        palette]
       #_(ctrl/checkbox settings "Dispersion" [:dispersion])])))

(sketch/definition radial-mosaic
  {:created-at "2021-05-15"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount page "sketch-host"))

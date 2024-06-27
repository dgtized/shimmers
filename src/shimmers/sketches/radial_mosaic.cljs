(ns shimmers.sketches.radial-mosaic
  (:require
   [shimmers.common.palette :as palette]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
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
(defn rv [x y]
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
  (palette/by-names
   [:shell-blue-yellow-grey
    :purple-shell-brown
    :shell-grey-blues
    :shell-aqua-blue-green
    :slate-shell-red-tan-yellow
    ;; some bolder palettes
    :shell-grey-blues-bold
    :yellow-blue-slate-grey-red
    :slate-black-green-forest-blue
    :red-black-yellow-grey-blue
    :orange-black-blue-shell-red
    :orange-maroon-blues
    :blues-orange-black-shell]))

(defn palette-sequence [palette segments]
  (let [multiple (let [m (sm/factors segments 12)]
                   (if (empty? m)
                     1
                     (dr/rand-nth m)))
        colors (into palette ["white" "white"])
        row-palette (repeatedly multiple #(dr/rand-nth colors))]
    (if (and (even? multiple) (> multiple 2) (dr/chance 0.33))
      (let [s (take (/ multiple 2) row-palette)]
        (concat s (reverse s)))
      row-palette)))

(comment (palette-sequence (:colors (first palettes)) 21))

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
  (->> [{:origin (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5)
         :radius (range 6 (int (* 0.5 height)))}
        {:origin (rv (dr/rand-nth [0.33 0.66]) 0.5)
         :radius (range 6 (int (* 0.6 width)))}
        {:origin (rv (dr/rand-nth [0.2 0.3 0.7 0.8]) (dr/rand-nth [0.33 0.4 0.6 0.66]))
         :radius (range 6 (int (* (dr/rand-nth [0.6 0.7 0.8 0.9]) width)))}]
       dr/rand-nth
       (merge {:palette (:colors (dr/rand-nth palettes))
               ;; TODO: set arc0 to arc1 to be close to a far corner from
               ;; center? Also, Consider setting a single theta with a radial
               ;; width and displace more the closer the piece is to theta?
               :displacement {:arc0 -0.5 :arc1 0.5 :percent 1.0 :force 0.3}})))

(defn page []
  (let [{:keys [palette] :as params} (mosaic-params)]
    (fn []
      [:<>
       [:div.canvas-frame
        (csvg/svg-timed {:id "scene"
                         :width width
                         :height height}
          [(scene params)])
        [usvg/download-shortcut "scene" "radial-mosaic"]]
       [:div.contained
        [:div.evencols
         [view-sketch/generate :radial-mosaic]
         [:div.readable-width
          [palette/as-svg {} palette]
          #_(ctrl/checkbox settings "Dispersion" [:dispersion])
          [:p "A circle is chopped into a set of radial arcs, ascending from the
        origin. Each arc is broken up into a number of segments proportional to
        the arc length. Find the common multiples between the number of segments
        and 12, and pick one randomly. That factor is used to pick a list of
        colors from a source palette for that particular row, which are then
        cycled across all segments in the arc, subdviding evenly as it's an even
        divisor."]]]]])))

(sketch/definition radial-mosaic
  {:created-at "2021-05-15"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount page))

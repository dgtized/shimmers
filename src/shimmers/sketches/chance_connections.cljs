(ns shimmers.sketches.chance-connections
  (:require
   [shimmers.algorithm.chaikin :as chaikin]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce ui-state
  (ctrl/state
   {:show-points false
    :show-intersections false
    :vary-width true
    :untangle true
    :chaikin false
    :depth 3}))

;; TODO: remove single intersection loops somehow? lookahead from an edge, if it
;; crosses only one other edge and no other intersecting edges in between,
;; splice connection and reverse the loop to connect without a loop?

;; Find all edge intersections, but include their index in the original edges?
;; Not sure how to deal with the edges that cross most of the sketch.
;; Basically need to unsnarl/planarize the connections

;; Also worth trying to find an euler tour?

;; Maybe cross-hatch the connections between each side of the resulting maze?
;; Or draw parallel lines to some edges, or mix and match between smooth and blocky?

;; Might want to remove extremly sharp hairpin angles?

(defn path-segments [points]
  (loop [path (vec (take 1 points))
         points (rest points)]
    (if (seq points)
      (let [current (last path)
            next (apply min-key (fn [v] (g/dist-squared current v))
                        points)]
        (recur (conj path next)
               (remove (fn [v] (tm/delta= next v)) points)))
      path)))

(defn intersections [points]
  (->> points
       (partition 2 1)
       (map-indexed vector)
       cs/non-consecutive-pairs
       (keep
        (fn [[[idx0 [a b]] [idx1 [c d]]]]
          (let [{type :type isec :p} (isec/intersect-line2-line2? a b c d)]
            (when (= type :intersect)
              {:isec isec :segments [[a b] [c d]] :indices [idx0 idx1]}))))))

(defn overlaps? [[a b] [c d]]
  (and (<= a d) (>= b c)))

(defn covers? [cover inside]
  (let [[a b] cover
        [c d] inside]
    (and (>= c a) (<= d b))))

;; Concept is to find ranges that are non-overlapping and non-covering, as those
;; can be flipped in direction to untangle a single intersection point.
;; FIXME: algorithm is buggy, particularly for termination case but also
;; sometimes finds false positives.
(defn simple-cycles [intervals]
  (loop [simple []
         ivals (rest intervals)
         current (first intervals)]
    ;; (println current ivals simple)
    (if (empty? ivals)
      (if current
        (conj simple current)
        simple)
      (let [[next & ivals']
            (remove (fn [x] (and (overlaps? current x)
                                (not (covers? current x))))
                    ivals)]
        (recur (if (overlaps? current next)
                 simple
                 (conj simple current))
               ivals'
               next)))))

(defn untangle [points intervals]
  (println "untangle " intervals (count points))
  (reduce
   (fn [pts [start end]]
     (vec (concat (subvec pts 0 (inc start))
                  (vec (reverse (subvec pts (inc start) (inc end))))
                  (subvec pts (inc end) (count pts)))))
   (vec points)
   intervals))

;; Need a better partitioning approach. Something like `k` edges, separate into ranges like:
;; {[0 5] 1, [5 8] 0.5, [8, 10] 1.2}
;; and then apply that over the edges at draw time. This is effectively a range
;; version of `dr/density-range`. Partitions would then be deterministic to edge
;; counts and could be applied in sequence from the beginning of the path,
;; unraveled or not.
(defn partition-range [n mu sd]
  (keep (fn [[t0 t1]]
          (let [lower (int (* t0 n))
                upper (int (* t1 n))]
            (when (> upper 0)
              [lower upper])))
        (partition 2 1 (dr/gaussian-range (/ mu n) (/ sd n) true))))

(defn segmentize [points parts]
  (loop [groups [] points points parts parts]
    (if (empty? parts)
      (if (empty? points)
        groups
        (conj groups points))
      (let [[start stop] (first parts)
            len (- stop start)]
        (recur (conj groups (take (inc len) points))
               (drop len points)
               (rest parts))))))

(comment (segmentize (range 20) (partition-range 20 1 3)))

(defn draw-segment [points attribs]
  (csvg/path (into [[:M (first points)]]
                   (map (fn [v] [:L v]) (rest points)))
             attribs))

(defn point-path [{:keys [chaikin depth vary-width
                          show-points show-intersections]}
                  points]
  (csvg/group {:stroke-width 1.5}
    (let [path (if chaikin
                 (chaikin/chaikin 0.2 false depth points)
                 points)]
      (if vary-width
        (for [segment (segmentize path (partition-range (count path) 4 2))]
          (draw-segment segment
                        {:stroke-width (tm/roundto (dr/random 1.0 8.0) 0.5)}))
        (draw-segment points {:stroke-width 1.0})))
    (when show-points
      (csvg/group {:fill "black"}
        (for [p points]
          (gc/circle p 2.0))))
    (when show-intersections
      (let [intersects (intersections points)]
        (println "indices" (map :indices intersects))
        (println (simple-cycles (map :indices intersects)))
        (csvg/group {:fill "black"}
          (for [{:keys [isec indices]} intersects]
            (csvg/group {} (gc/circle isec 2.0)
              (csvg/center-label
               (tm/+ isec (gv/vec2 0 -12))
               (str indices)
               {:font-size 12}))))))))

(defn remove-cycles [path]
  (cs/iterate-fixed-point
   (fn [pts]
     (untangle pts (simple-cycles (map :indices (intersections pts)))))
   path))

(defn scene [bounds points settings]
  (csvg/svg-timed {:width (g/width bounds)
                   :height (g/height bounds)
                   :stroke "black"
                   :fill "none"}
    (point-path settings
                (if (:untangle settings)
                  (remove-cycles points)
                  points))))

(defn page []
  (let [bounds (rect/rect 0 0 width height)
        path (path-segments (rp/poisson-disc-sampling (g/scale-size bounds 0.95) 90))]
    (fn []
      [:<>
       [:div.canvas-frame [scene bounds path @ui-state]]
       [:div.contained
        [:div.flexcols {:style {:justify-content :space-evenly :gap "0 0"}}
         [:div
          [:p]
          [view-sketch/generate :chance-connections]
          [:p]
          [:div {:style {:min-width "20em"}}
           [ctrl/checkbox ui-state "Untangle" [:untangle]]
           [ctrl/checkbox ui-state "Vary Widths" [:vary-width]]
           [ctrl/checkbox ui-state "Show Points" [:show-points]]
           [ctrl/checkbox ui-state "Show Intersections" [:show-intersections]]
           [:div.flexcols {:style {:gap "0px 1.5em"}}
            [ctrl/checkbox ui-state "Chaiken Smooth" [:chaikin]]
            (when (:chaikin @ui-state)
              [ctrl/numeric ui-state "Depth" [:depth] [1 6 1]])]]]
         [:div
          [:p.readable-width
           "Create a set of random points using poisson disc sampling. Pick the
     first point to start and then greedily take the next closest point in the
     set from the current position until the set is exhausted."]
          [:p.readable-width
           "As often there are intersections in the resulting path, untangle
           will find all self-intersections, and remove any simple loops it can
           find. Simple loops are sequences like A-B-C-D where A-B and C-D
           intersect. These can be unwound by connecting opposite ends of the
           loop and reversing the middle, ie a sequence like A-C-B-D. This is
           applied repeatedly until all simple loops are removed."]
          [:p.readable-width
           "A few paths still have loops where the entry and exit
           are from two intersections, those are not currently corrected."]]]]])))

(sketch/definition chance-connections
  {:created-at "2023-06-01"
   :tags #{}
   :type :svg}
  (ctrl/mount (page)))

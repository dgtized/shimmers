(ns shimmers.sketches.divisible
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.bounded-shapes :as bounded]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn int-rect [{[x y] :p [w h] :size}]
  (rect/rect (int x) (int y) (int w) (int h)))

(defn generate-boxes [bounds]
  (iterate (fn [existing]
             (let [candidate (-> bounds
                                 (bounded/rectangle (tm/roundto (dr/random 0.05 0.3) 0.05)
                                                    (tm/roundto (dr/random 0.05 0.3) 0.05))
                                 int-rect)
                   margin (g/scale-size candidate 1.4)]
               (if (some (fn [box] (when (collide/overlaps? margin box) box))
                         existing)
                 existing
                 (conj existing candidate))))
           []))

(defn left-side [rect]
  (gl/line2 (rect/bottom-left rect)
            (gv/vec2 (rect/left rect) (rect/top rect))))

(defn right-side [rect]
  (gl/line2 (gv/vec2 (rect/right rect) (rect/bottom rect))
            (rect/top-right rect)))

(defn top-side [rect]
  (gl/line2 (gv/vec2 (rect/left rect) (rect/top rect))
            (rect/top-right rect)))

(defn bottom-side [rect]
  (gl/line2 (rect/bottom-left rect)
            (gv/vec2 (rect/right rect) (rect/bottom rect))))

(comment
  (right-side (rect/rect 5))
  (left-side (rect/rect 5))
  (map g/bounds (lines/cut-polygon (rect/rect 10) (left-side (rect/rect 2 0 6 10)))))

;; this doesn't work because a punch might intersect with more than one rectangle
(defn punch-out [rect punch]
  (if (collide/bounded? rect punch)
    (let [clip (g/scale-size (right-side punch) 1000)
          cuts (mapv g/bounds (lines/cut-polygon rect clip))]
      ;; (println {:rect rect :clip clip :cuts cuts})
      (mapcat (fn [s] (if (collide/bounded? s punch)
                       (square/difference s punch)
                       [s]))
              cuts))
    [rect]))

(defn convert-rectangles [shapes]
  (map (fn [s] (if (= 4 (count (g/vertices s)))
                (g/bounds s)
                s))
       shapes))

(defn joined-polygons [polygons]
  (println polygons)
  (->> polygons
       (cs/iterate-cycles
        3
        (fn [polygons] (reduce (fn [shapes s]
                                (let [last-shape (last shapes)]
                                  (if-let [joined (and last-shape (lines/join-polygons last-shape s))]
                                    (conj (butlast shapes) joined)
                                    (conj shapes s))))
                              nil polygons)))
       convert-rectangles))

(defn reduce-overlapping [shapes overlap]
  (let [overlapping (filter (fn [s] (collide/overlaps? overlap s)) shapes)
        remaining (remove (fn [s] (collide/overlaps? overlap s)) shapes)]
    (->> (for [[p q](g/edges overlap)]
           (g/scale-size (gl/line2 p q) 1000))
         (lines/slice-polygons (joined-polygons overlapping))
         convert-rectangles
         (concat remaining))))

(defn shapes [bounds]
  (let [punches (->> bounds
                     generate-boxes
                     (drop-while (fn [s] (< (count s) (dr/random-int 5 22))))
                     first
                     (sort-by (fn [s] (rect/right s)) <))
        remaining (reduce (fn [rects box]
                            (mapcat (fn [r] (punch-out r box)) rects))
                          [bounds]
                          punches)]
    (concat (reduce reduce-overlapping
                    remaining
                    punches)
            (map-indexed (fn [i s] (let [fill (csvg/hsv (/ i (count punches)) 1.0 0.5 0.33)]
                                    (vary-meta s assoc :fill fill)))
                         punches))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [:div
      [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
       [view-sketch/generate :divisible]]
      [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
       [:div.readable-width
        [:p "Experimenting with dividing a rectangle by punching
     out a set of rectangles inside of it, and then calculating the set of
     rectangles remaining which would tile the space."]
        [:p "Current approach is to recursively calculate difference of each
      rectangle that remains that contains one of the punched out rectangles.
      Unfortunately, this fails because remaining rectangle may overlap more
      then one existing region."]
        [:p "After punching out every pane, any remaining cases which are not
       bounded by an existing rectangle are sliced out of a polygon joined from
       the overlap. This results in some interesting non-local jagged edges."]
        [:p "There also appears to be a bug (possibly in join polygons), which
       results in some diagonals, and occasionally a punching pane is still
       overlapped."]
        [:p "A possible solution is to investigate the "
         [:a {:href "https://en.wikipedia.org/wiki/Guillotine_cutting"}
          "guillotine cutting"] " problem."]]]]]))

(sketch/definition divisible
    {:created-at "2023-11-21"
     :tags #{}
     :type :svg}
  (ctrl/mount page))

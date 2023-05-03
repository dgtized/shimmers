(ns shimmers.sketches.intersecting-chords
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-circle [{p :p [w h] :size}]
  (let [r (-> (dr/gaussian (/ h 16) (/ h 20))
              (max (/ h 30))
              (min (/ h 3)))]
    (gc/circle (tm/+ p (gv/vec2 (dr/random r (- w r))
                                (dr/random r (- h r))))
               r)))

(defn legal-candidate
  [circletree
   {:keys [bounds gen-circle min-spacing max-spacing]}]
  (let [candidate (gen-circle)]
    (when (geometry/contains-circle? bounds candidate)
      (if-let [near (saq/closest-circle circletree candidate)]
        (let [d (saq/circle-overlap near candidate)]
          (when (and (not (collide/bounded? near candidate))
                     (< min-spacing d max-spacing))
            candidate))
        candidate))))

(defn circle-pack [{[w h] :size :as bounds} n]
  (pack/add-circles
   (saq/circletree bounds)
   (fn [tree]
     (legal-candidate
      tree
      {:bounds (g/scale-size bounds 0.95)
       :gen-circle (partial make-circle bounds)
       :min-spacing (- (* 0.1 (min w h)))
       :max-spacing (* 0.05 (min w h))}))
   n))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :background (q/create-graphics (q/width) (q/height))
     :circletree (circle-pack bounds 15)
     :hue (dr/rand-nth [0.0 0.45 0.6 0.8])
     :t 0.0}))

(defn rescale [bounds t {:keys [r] :as circle}]
  (or (when-let [{:keys [R dr t0 dt]} (:R circle)]
        (let [circle' (assoc circle :r (+ R (* dr (Math/sin (+ (* t dt) t0)))))]
          (if (geometry/contains-circle? bounds circle')
            circle'
            (update circle' :p
                    (fn [p]
                      (let [contact (g/closest-point bounds p)
                            dist (g/dist p contact)]
                        ;; shove point away from wall by distance between radius
                        ;; and contact point distance
                        (tm/+ p (tm/normalize (tm/- p contact) (- (:r circle') dist)))))))))
      (assoc circle :R {:R r
                        :dr (dr/random (* 0.2 r) (* 0.4 r))
                        :t0 (dr/random-tau)
                        :dt (dr/gaussian 1 0.1)})))

(defn move [bounds circle]
  (or (when-let [v (:v circle)]
        (let [circle' (update circle :p tm/+ v)]
          (when (geometry/contains-circle? bounds circle')
            circle')))
      (assoc circle :v (dr/randvec2 (max 0.1 (dr/gaussian 0.35 0.06))))))

(defn update-state [{:keys [bounds circletree t] :as state}]
  (let [bounds' (g/scale-size bounds 0.95)
        circles (->> circletree
                     saq/all-data
                     (map (partial rescale bounds' t))
                     (map (partial move bounds')))
        with-eraser
        (if (some (fn [c] (:eraser c)) circles)
          circles
          (let [k (int (* 0.6 (count circles)))]
            (assoc-in (vec (sort-by :r circles)) [k :eraser] true)))]
    (-> state
        (assoc :circletree
               (saq/add-to-circletree (saq/circletree bounds) with-eraser))
        (update :t + 0.01))))

(defonce ui-state
  (ctrl/state {:show-circles false
               :show-closest false
               :show-chords false
               :show-background true
               :show-eraser false
               :add-eraser true
               :jagged-eraser true}))

(defn draw-eraser [circle jagged]
  (q/no-stroke)
  (q/fill 1.0 0.1)
  (let [triangle (if jagged
                   (-> (triangle/inscribed-equilateral circle (dr/random-tau))
                       (g/translate (dr/randvec2 (* 0.66 (:r circle)))))
                   (triangle/inscribed-equilateral circle 0))]
    (cq/draw-triangle (g/vertices triangle)))
  (q/no-fill))

;; Idea: Assign circles as chord or radius type at creation. Then adjust the
;; draw routine to connect chord/chord and radius/radius as expected. For
;; chord/radius, draw the line from the center to one or both intersecting
;; point?
(defn draw [{:keys [t circletree background hue]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/with-graphics background
    (q/color-mode :hsl 1.0)
    (q/no-fill)
    (q/stroke-weight 0.5)
    (q/stroke (eq/unit-sin t) 0.2))
  (let [{:keys [show-circles show-closest show-chords show-background
                show-eraser add-eraser jagged-eraser]}
        @ui-state]
    (when show-background
      (q/image background 0 0))
    (doseq [{p :p :as circle} (saq/all-data circletree)]
      (when show-circles
        (q/stroke-weight 0.66)
        (q/stroke 0.66)
        (cq/circle circle))
      (when (and add-eraser (:eraser circle))
        (when show-eraser
          (q/stroke 0.66)
          (q/stroke-weight 0.66)
          (cq/draw-triangle (g/vertices (triangle/inscribed-equilateral circle 0))))
        (q/with-graphics background
          (draw-eraser circle jagged-eraser)))
      (q/stroke-weight 1.0)
      (doseq [nearby (saq/k-nearest-neighbors circletree 3 p)]
        (when-let [neighbor (g/get-point-data nearby)]
          (if (collide/overlaps? circle neighbor)
            (when-let [isecs (isec/intersect-circle-circle? circle neighbor)]
              (when show-chords
                (q/stroke 0.0)
                (apply q/line isecs))
              (q/with-graphics background
                (q/stroke (eq/unit-sin t) 0.2)
                (apply q/line isecs)))
            (let [q (:p neighbor)
                  d (g/dist p q)]
              (when show-closest
                (q/stroke 0.0 0.5 0.25)
                (q/line (:p circle) (:p neighbor)))
              (when (< d (* 2.0 (:r circle)))
                (q/with-graphics background
                  (q/stroke hue 0.35 (* 0.5 (eq/unit-cos t)) 0.15)
                  (q/line p q))))))))))

(defn ui-controls []
  [:div.flexcols
   [:div {:style {:width "20ch"}}
    [:p]
    (ctrl/checkbox ui-state "Show Circles" [:show-circles])
    (ctrl/checkbox ui-state "Show Closest" [:show-closest])
    (ctrl/checkbox ui-state "Show Chords" [:show-chords])
    (ctrl/checkbox ui-state "Show Background" [:show-background])
    (ctrl/checkbox ui-state "Add Eraser" [:add-eraser])
    (ctrl/checkbox ui-state "Show Eraser" [:show-eraser])
    (ctrl/checkbox ui-state "Jagged Eraser" [:jagged-eraser])]
   [:div {:style {:width "75ch"}}
    [:p "Prompt: Genuary2023 Day 4 - Intersections"]
    [:p "For the closest three neighbors of a circle, if they intersect, draw
    the chord between the intersection points of the two circles in monochrome.
    Otherwise, draw the connecting line to the neighbor in a pre-selected color
    if the distance is less than the diameter of the circle. Time modulates the
    radius of each circle, and the brightness of each line. The speed of each
    circle changes everytime it bounces off the canvas boundary"]
    [:p "Finally, one circle is labeled the eraser, and returns the canvas to
    emptiness to keep some negative space."]
    [:p "This is a variation on Casey Reas' technique of drawing a line between
    two circles if they intersect."]]])

(defn page []
  [:div
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [ui-controls]]])

(sketch/definition intersecting-chords
  {:created-at "2023-01-04"
   :tags #{:genuary2023}
   :type :quil}
  (ctrl/mount page))

(ns shimmers.sketches.intersecting-chords
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
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

(defn pack-candidates
  [circletree legal-candidate n]
  (loop [i 0 tree circletree]
    (if (>= i n)
      tree
      (if-let [circle (legal-candidate tree)]
        (recur (inc i) (saq/add-point tree (:p circle)
                                      (if (= i 0)
                                        (assoc circle :eraser true)
                                        circle)))
        (recur i tree)))))

(defn circle-pack [{[w h] :size :as bounds} n]
  (pack-candidates (saq/circletree bounds)
                   (fn [tree]
                     (legal-candidate
                      tree
                      {:bounds (g/scale-size bounds 0.95)
                       :gen-circle (partial make-circle bounds)
                       :min-spacing (- (* 0.1 (min w h)))
                       :max-spacing (* 0.05 (min w h))}))
                   n))

(defn rebuild-tree [bounds circles]
  (reduce (fn [t c] (saq/add-point t (:p c) c))
          (saq/circletree bounds)
          circles))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :background (q/create-graphics (q/width) (q/height))
     :circletree (circle-pack bounds 15)
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
                        :t0 (dr/random eq/TAU)
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
                     (map (partial move bounds')))]
    (-> state
        (assoc :circletree (rebuild-tree bounds circles))
        (update :t + 0.01))))

(defonce ui-state
  (ctrl/state {:show-circles false
               :show-closest false
               :show-chords false
               :show-background true
               :add-eraser true}))

(defn draw [{:keys [t circletree background]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/with-graphics background
    (q/color-mode :hsl 1.0)
    (q/no-fill)
    (q/stroke-weight 0.5)
    (q/stroke (eq/unit-sin t) 0.2))
  (let [{:keys [show-circles show-closest show-chords show-background add-eraser]} @ui-state]
    (when show-background
      (q/image background 0 0))
    (doseq [{p :p :as circle} (saq/all-data circletree)]
      (when show-circles
        (q/stroke-weight 0.66)
        (q/stroke 0.66)
        (cq/circle circle))
      (when (and add-eraser (:eraser circle))
        (q/with-graphics background
          (let [triangle (triangle/inscribed-equilateral circle 0.0)]
            (q/no-stroke)
            (q/fill 1.0 1.0)
            (cq/draw-triangle (g/vertices triangle))
            (q/no-fill))))
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
                  (q/stroke 0.0 0.35 (* 0.5 (eq/unit-cos t)) 0.15)
                  (q/line p q))))))))))

(defn ui-controls []
  [:div
   (ctrl/checkbox ui-state "Show Circles" [:show-circles])
   (ctrl/checkbox ui-state "Show Closest" [:show-closest])
   (ctrl/checkbox ui-state "Show Chords" [:show-chords])
   (ctrl/checkbox ui-state "Show Background" [:show-background])
   (ctrl/checkbox ui-state "Add Eraser" [:add-eraser])])

(sketch/defquil intersecting-chords
  :created-at "2023-01-04"
  :tags #{:genuary2023}
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.differential-growth
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; References:
;; https://medium.com/@jason.webb/2d-differential-growth-in-js-1843fd51b0ce
;; https://inconvergent.net/generative/differential-line/
;; https://inconvergent.net/2016/shepherding-random-growth/

;; TODO: what happens to algorithm with gravity/wind forces in addition to the
;; neighbor points
(defonce defo (debug/state))

(defn path-split [{:keys [max-pop split-threshold split-chance jitter]} points]
  (let [n (count points)
        split (/ split-chance n)]
    (if (> n max-pop)
      points
      (concat (apply concat
                     (for [[p q] (partition 2 1 points)]
                       (if (dr/chance (* (/ (g/dist p q) split-threshold) split))
                         [p (tm/+ (tm/mix p q (dr/random)) (dr/jitter jitter))]
                         [p])))
              [(last points)]))))

(defn rejection-force [{:keys [neighborhood repulsion]} quad from]
  (let [forces (->> (spatialtree/select-with-circle quad from neighborhood)
                    (remove #(tm/delta= from %))
                    (map (fn [p] (tm/* (tm/- from p) (/ repulsion (g/dist from p))))))]
    (if (empty? forces)
      (gv/vec2)
      (reduce tm/+ forces))))

;; Push inward towards the center of a convex boundary
(defn boundary-force [bounds position]
  (let [[obstacle _] (gu/closest-point-on-segments position (g/edges bounds))]
    (tm/normalize (tm/- (g/centroid bounds) obstacle)
                  (* 50 (tm/clamp01 (/ 1 (g/dist-squared position obstacle)))))))

(defn apply-force [{:keys [force-scale attraction alignment] :as config}
                   quad bounds [before after] point]
  (let [forces [(tm/* (tm/- before point) attraction)
                (tm/* (tm/- after point) attraction)
                (tm/* (tm/- (tm/mix before after 0.5) point) alignment)
                (rejection-force config quad point)]
        goal (reduce tm/+ point forces)]
    (tm/+ (tm/mix point goal force-scale) (boundary-force bounds goal))))

;; only applies forces to a sampling of the points each frame
(defn apply-forces [{:keys [percent-update] :as config} bounds points]
  (let [quad (reduce (fn [q p] (g/add-point q p p)) (spatialtree/quadtree bounds) points)]
    (concat [(first points)]
            (->> points
                 (partition 3 1)
                 (map (fn [[a b c]]
                        (if (dr/chance percent-update)
                          (apply-force config quad bounds [a c] b)
                          b))))
            [(last points)])))

(defn natural-selection [{:keys [max-pop]} points]
  (let [n (count points)]
    (cond (and (> n (* 0.99 max-pop)) (dr/chance 0.3))
          (dr/random-sample 0.90 points)
          (and (> n (* 0.90 max-pop)) (dr/chance 0.01))
          (dr/random-sample 0.99 points)
          :else
          points)))

(defn bounds-check [bounds points]
  (map #(v/clamp-bounds bounds %) points))

;; What would happen if each node was a circle with radius and that adjusted force calculations
(defn path-update [{:keys [points]}]
  (let [bounds (cq/screen-rect 0.95)
        config {:force-scale 0.33
                :percent-update 0.33
                :attraction 0.4
                :alignment 0.2
                :split-threshold (cq/rel-w 0.03)
                :split-chance 0.5
                :jitter (cq/rel-w 0.01)
                :neighborhood (cq/rel-w 0.05)
                :repulsion 0.8
                :max-pop 1000}]
    (->> points
         (path-split config)
         (bounds-check bounds)
         (apply-forces config bounds)
         (natural-selection config)
         gl/linestrip2)))

;; TODO: correctly handle loops in apply-forces and path-split and try it with a circle
(defn setup []
  (q/color-mode :hsl 1.0)
  {:path (->> [[(cq/rel-vec 0.05 0.5) (cq/rel-vec 0.95 0.5)]
               [(cq/rel-vec 0.05 0.95) (cq/rel-vec 0.05 0.05)
                (cq/rel-vec 0.95 0.05) (cq/rel-vec 0.95 0.95)]
               (let [points (g/vertices (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.1)) 36)]
                 (conj points (first points)))]
              dr/rand-nth
              gl/linestrip2)})

(defn update-state [state]
  (reset! defo {})
  (update state :path path-update))

(defn draw-path [vertices]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (q/stroke 0.0)
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape)
  (q/fill 0.0)
  (doseq [v vertices]
    (cq/circle v 3)))

(defn draw-continous-path [vertices]
  ;; (q/background 1.0 0.05)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (q/stroke 0.0 0.01)
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape)
  (q/fill 0.0 0.01)
  (doseq [v vertices]
    (cq/circle v 3)))

(defonce ui-state (ctrl/state {:persistent false}))

(defn draw [{:keys [path]}]
  (let [{:keys [points]} path
        draw-mode (if (get-in @ui-state [:persistent])
                    draw-continous-path
                    draw-path)]
    (swap! defo assoc :count (count points))
    (draw-mode points)))

(defn ui-controls []
  [:div.flexcols
   (ctrl/container {:style {:width "15em"}}
                   [:h4 "Controls"]
                   (ctrl/checkbox ui-state "Persistent Background" [:persistent]))
   [:div
    (debug/display defo)]])

(sketch/defquil differential-growth
  :created-at "2022-02-23"
  :size [800 600]
  :on-mount (fn [] (ctrl/mount ui-controls))
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

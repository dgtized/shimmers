(ns shimmers.sketches.differential-growth
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; References:
;; https://medium.com/@jason.webb/2d-differential-growth-in-js-1843fd51b0ce
;; https://inconvergent.net/generative/differential-line/
;; https://inconvergent.net/2016/shepherding-random-growth/

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

(defn apply-force [{:keys [force-scale attraction alignment] :as config}
                   quad [before after] point]
  (tm/mix point
          (reduce tm/+ point
                  [(tm/* (tm/- before point) attraction)
                   (tm/* (tm/- after point) attraction)
                   (tm/* (tm/- (tm/mix before after 0.5) point) alignment)
                   (rejection-force config quad point)])
          force-scale))

;; only applies forces to a sampling of the points each frame
(defn apply-forces [{:keys [percent-update] :as config} bounds points]
  (let [quad (reduce (fn [q p] (g/add-point q p p)) (spatialtree/quadtree bounds) points)]
    (concat [(first points)]
            (->> points
                 (partition 3 1)
                 (map (fn [[a b c]]
                        (if (dr/chance percent-update)
                          (apply-force config quad [a c] b)
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

;; What would happen if updates only happen to a percentage of nodes?
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
  (q/no-fill)
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape)
  (q/fill 0.0)
  (doseq [v vertices]
    (cq/circle v 3)))

(defn draw [{:keys [path]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (let [{:keys [points]} path]
    (swap! defo assoc :count (count points))
    (draw-path points)))

(sketch/defquil differential-growth
  :created-at "2022-02-23"
  :size [800 600]
  :on-mount (fn [] (debug/mount defo))
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

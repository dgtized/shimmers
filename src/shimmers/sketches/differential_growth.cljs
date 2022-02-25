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

;; FIXME: split-chance is proportional to quantity of points
(defn path-split [{:keys [split-threshold split-chance jitter]} points]
  (concat (apply concat
                 (for [[p q] (partition 2 1 points)]
                   (if (or (> (g/dist p q) split-threshold)
                           (dr/chance split-chance))
                     [p (tm/+ (tm/mix p q (dr/random)) (dr/jitter jitter))]
                     [p])))
          [(last points)]))

(defn rejection-force [{:keys [neighborhood repulsion]} quad from]
  (let [forces (->> (spatialtree/select-with-circle quad from neighborhood)
                    (remove #(tm/delta= from %))
                    (map (fn [p] (tm/* (tm/- from p) (/ repulsion (g/dist from p))))))]
    (if (empty? forces)
      (gv/vec2)
      (reduce tm/+ forces))))

(defn apply-force [{:keys [attraction alignment] :as config} quad [before after] point]
  (reduce tm/+ point
          [(tm/* (tm/- before point) attraction)
           (tm/* (tm/- after point) attraction)
           (tm/* (tm/- (tm/mix before after 0.5) point) alignment)
           (rejection-force config quad point)]))

(defn apply-forces [config bounds points]
  (let [quad (reduce (fn [q p] (g/add-point q p p)) (spatialtree/quadtree bounds) points)]
    (concat [(first points)]
            (for [[a b c] (partition 3 1 points)]
              (apply-force config quad [a c] b))
            [(last points)])))

(defn natural-selection [{:keys [max-pop]} points]
  (if (> (count points) max-pop)
    (dr/random-sample 0.95 points)
    points))

(defn bounds-check [bounds points]
  (map #(v/clamp-bounds bounds %) points))

(defn path-update [{:keys [points]}]
  (let [bounds (cq/screen-rect 0.95)
        config {:attraction 0.1
                :alignment 0.1
                :split-threshold (cq/rel-w 0.02)
                :split-chance 0.0005
                :jitter (cq/rel-w 0.01)
                :neighborhood (cq/rel-w 0.05)
                :repulsion 0.2
                :max-pop 512}]
    (->> points
         (path-split config)
         (bounds-check bounds)
         (apply-forces config bounds)
         (natural-selection config)
         gl/linestrip2)))

;; TODO: correctly handle loops in apply-forces and path-split and try it with a circle
(defn setup []
  (q/color-mode :hsl 1.0)
  {:path (gl/linestrip2 [(cq/rel-vec 0.05 0.5) (cq/rel-vec 0.95 0.5)])})

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

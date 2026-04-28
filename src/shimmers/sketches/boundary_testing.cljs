(ns shimmers.sketches.boundary-testing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.math.core :as tm]))

(defn interesting-triangle [a b]
  (let [m (tm/mix a b (dr/random))
        perp (g/scale (g/rotate (tm/- b a) (* eq/TAU 0.25 (dr/rand-nth [-1 1]))) (dr/random 0.2 1.0))]
    (gt/triangle2 a (tm/+ m perp) b)))

(defn random-shape []
  (let [point-gen (fn [] (cq/rel-vec (dr/random 0.3 0.7) (dr/random 0.3 0.7)))]
    (dr/weighted
     [[(gl/line2 (point-gen) (point-gen)) 1.0]
      [(interesting-triangle (point-gen) (point-gen)) 1.0]
      [(rect/rect (point-gen) (point-gen)) 1.0]
      [(gp/polygon2 (repeatedly (dr/random-int 3 9) point-gen)) 1.0]
      [(gc/circle (point-gen) (cq/rel-h (dr/random 0.1 0.25)))
       1.0]])))

(defn object [shape pos vel]
  {:shape shape
   :pos pos
   :prev (tm/- pos vel)
   :spin (dr/random -0.005 0.005)
   :angle 0})

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-fill)
  {:objects (repeatedly 12 (fn [] (object (random-shape) (cq/rel-vec (dr/random 0.2 0.8) (dr/random 0.2 0.8))
                                         (dr/randvec2 1.0))))
   :t (q/millis)})

(defn update-object [t dt {:keys [shape pos prev angle spin] :as object}]
  (update object :angle + (* spin dt)))

(defn update-state [{:keys [t] :as state}]
  (let [dt (- (q/millis) t)]
    (-> state
        (update :objects (partial mapv (partial update-object t dt)))
        (update :t + dt))))

(defn draw [{:keys [objects]}]
  (q/background 1.0)
  (doseq [{:keys [shape pos angle]} objects]
    (qdg/draw (g/center (g/rotate shape angle) pos))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition boundary-testing
  {:created-at "2026-04-27"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

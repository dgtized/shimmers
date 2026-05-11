(ns shimmers.sketches.boundary-testing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state {}))

(defn gen-circle []
  (gc/circle (cq/rel-h (min (dr/random 0.025 0.25) (dr/random 0.025 0.25)))))

(defn interesting-triangle [a b]
  (let [m (tm/mix a b (dr/random))
        perp (g/scale (g/rotate (tm/- b a) (* eq/TAU 0.25 (dr/rand-nth [-1 1])))
                      (dr/random 0.2 0.9))]
    (gt/triangle2 a (tm/+ m perp) b)))

(defn interesting-polygon []
  (let [xs (g/vertices (g/as-polygon (gen-circle) (dr/random-int 3 9)))]
    (gp/polygon2 (for [vertice xs]
                   (tm/mix (gv/vec2) vertice (dr/random 0.25 1.33))))))

(defn random-shape []
  (let [point-gen (fn [] (cq/rel-vec (dr/random 0.35 0.65) (dr/random 0.35 0.65)))]
    (->> [[(gl/line2 (point-gen) (point-gen)) 1.0]
          [(interesting-triangle (point-gen) (point-gen)) 1.5]
          [(rect/rect (point-gen) (point-gen)) 1.5]
          [(interesting-polygon) 1.25]
          [(gen-circle) 1.5]]
         dr/weighted
         g/center)))

(defn object [shape pos vel]
  {:shape shape
   :pos pos
   :prev (tm/- pos vel)
   :spin (dr/random -0.0005 0.0005)
   :angle 0})

(defn object-at [{:keys [shape pos angle]}]
  (g/center (g/rotate shape angle) pos))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  {:bounds (cq/screen-rect 1.0)
   :objects
   (->> (fn [] (object (random-shape) (cq/rel-vec (dr/random 0.2 0.8) (dr/random 0.2 0.8))
                      (dr/randvec2 1.0)))
        (repeatedly (dr/random-int 8 23))
        (map-indexed (fn [idx obj] (assoc obj :idx idx)))
        vec)
   :t (q/millis)})

(defn update-object [bounds _t dt {:keys [pos prev _angle spin] :as object}]
  (let [vel (if (collide/bounded? bounds (object-at object))
              (tm/* (tm/- pos prev) (* 0.01 dt)) ;; FIXME: dampening acc
              (let [close (g/closest-point bounds pos)
                    center (g/centroid bounds)]
                (tm/* (tm/- center close) (* (/ 0.1 (g/dist center close)) dt))))]
    (-> object
        (update :pos tm/+ vel)
        (assoc :prev pos)
        (update :angle + (* spin dt)))))

(defn update-state [{:keys [bounds t] :as state}]
  (let [dt (- (q/millis) t)]
    (-> state
        (update :objects (partial mapv (partial update-object bounds t dt)))
        (update :t + dt))))

(defn draw [{:keys [objects]}]
  (reset! defo {})
  (q/background 1.0)
  (doseq [object objects
          :let [obj (object-at object)]]
    (q/stroke-weight 1.0)
    (q/stroke 0.0)
    (let [overlap (some (fn [other]
                          (when (and (not= (:idx object) (:idx other))
                                     (collide/overlaps? obj (object-at other)))
                            other))
                        objects)
          bounded (some (fn [other]
                          (when (and (not= (:idx object) (:idx other))
                                     (collide/bounded? (object-at other) obj))
                            other))
                        objects)]
      (when bounded
        (q/line (g/centroid (object-at object))
                (g/centroid (object-at bounded)))
        (q/stroke 0.575 0.75 0.5)
        (swap! defo update :bounded conj [(object-at bounded) obj]))
      (q/stroke-weight (if overlap 3.0 1.0)))
    (qdg/draw (object-at object))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [debug/display defo]])

(sketch/definition boundary-testing
  {:created-at "2026-04-27"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

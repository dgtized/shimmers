(ns shimmers.sketches.layered-layers
  (:require
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.bounded-shapes :as bounded]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn add-independent-shape [bounds shape-gen]
  (fn [{:keys [shapes] :as state}]
    (let [candidate (shape-gen bounds)]
      (if (and (collide/bounded? bounds candidate)
               (not-any? (fn [s]
                           (when (collide/overlaps? candidate s)
                             s))
                         shapes))
        (update state :shapes conj candidate)
        (update state :attempts dec)))))

(defn gen-shapes [add-shape-f n]
  (->> {:shapes [] :attempts 200}
       (iterate add-shape-f)
       (drop-while (fn [{:keys [shapes attempts]}]
                     (and (< (count shapes) n) (pos? attempts))))
       first
       :shapes))

(defn regular-polygons [n]
  (fn [{:keys [bounds levels]}]
    (let [_level (count levels)
          size (dr/random 0.025 0.125)
          displacement (dr/random 0.25 0.5)]
      (gen-shapes
       (add-independent-shape
        bounds
        (fn [_] (-> (poly/regular-n-gon n (* (g/width bounds) (dr/random 0.5 2.0) size))
                   (g/rotate (dr/random-tau))
                   (g/center (tm/+ (rv 0.5 0.5)
                                   (dr/randvec2 (* displacement (dr/random 0.5 1.0) (g/width bounds))))))))
       (dr/random-int 3 8)))))

(defn circles [{:keys [bounds]}]
  (let [R (max (g/width bounds) (g/height bounds))]
    (reduce
     (fn [circles pct]
       (let [radius (* R pct)
             r (max (dr/gaussian radius (* 0.05 radius)) (* 0.001 R))]
         (pack/circle-pack
          circles
          {:bounds bounds
           :candidates (int (/ 5 pct))
           :gen-circle (fn [] (bounded/circle-with-radius bounds r))
           :spacing (max (* 0.02 R) (* 0.1 radius))})))
     []
     [0.1 0.08 0.06 0.04 0.02 0.01])))

(defn gen-layer [state]
  (let [layer (dr/weighted [[(regular-polygons 3) 1.0]
                            [(regular-polygons 4) 2.0]
                            [(regular-polygons 5) 1.0]
                            [(regular-polygons 6) 1.0]
                            [circles 1.0]])
        shapes (layer state)]
    shapes))

(defn add-layer [state]
  (update state :levels conj (gen-layer state)))

(defn shapes [bounds palette]
  (let [colors (dr/shuffle (into palette ["none" "none"]))]
    (map-indexed (fn [i level]
                   (csvg/group {:fill (nth colors (mod i (count colors)))
                                :opacity 0.9}
                     level))
                 (:levels (nth (iterate add-layer {:bounds bounds :levels []}) 8)))))

(defn scene [{:keys [scene-id palette]}]
  (fn []
    (csvg/svg-timed {:id scene-id
                     :width width
                     :height height
                     :stroke "black"
                     :fill "white"
                     :stroke-width 0.5}
      (shapes (g/scale-size (csvg/screen width height) 0.975)
              palette))))

(defn explanation [{:keys [palette]}]
  [:div.evencols
   [:div.readable-width
    [:p "Genuary 2025 - Day 02 - Layers on layers upon layers"]]
   [:p
    [palette/as-svg {} palette]]])

(sketch/definition layered-layers
  {:created-at "2025-01-02"
   :tags #{:genuary2025}
   :type :svg}
  (ctrl/mount
   (let [palette (:colors (dr/rand-nth palette/db))]
     (usvg/page (assoc sketch-args
                       :palette palette
                       :explanation explanation)
                scene))))

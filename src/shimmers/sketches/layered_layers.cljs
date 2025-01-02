(ns shimmers.sketches.layered-layers
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
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

(defn boxes [{:keys [bounds levels]}]
  (let [level (count levels)]
    (gen-shapes
     (add-independent-shape
      bounds
      (fn [_] (-> (poly/regular-n-gon 4
                                     (* (g/width bounds) (dr/random 0.01 0.05) (- 7 level)))
                 (g/rotate (dr/random-tau))
                 (g/center (tm/+ (rv 0.5 0.5)
                                 (dr/randvec2 (* level 0.075 (g/width bounds))))))))
     (dr/random-int 3 8))))

(defn gen-layer [state]
  (let [layer (dr/weighted [[boxes 1.0]])
        shapes (layer state)]
    shapes))

(defn add-layer [state]
  (update state :levels conj (gen-layer state)))

(defn shapes [bounds]
  (map-indexed (fn [i level] (csvg/group {:fill (if (even? i) "white" "none")}
                              level))
               (:levels (nth (iterate add-layer {:bounds bounds :levels []}) 6))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes (g/scale-size (csvg/screen width height) 0.975))))

(defn explanation [_]
  [:p "Genuary 2025 - Day 02 - Layers on layers upon layers"])

(sketch/definition layered-layers
  {:created-at "2025-01-02"
   :tags #{:genuary2025}
   :type :svg}
  (ctrl/mount (usvg/page (assoc sketch-args
                                :explanation explanation)
                         scene)))

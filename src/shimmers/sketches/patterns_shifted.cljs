(ns shimmers.sketches.patterns-shifted
  (:require
   [clojure.set :as set]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

(defonce ui-state (ctrl/state {:debug false}))
(defonce defo (debug/state []))

(def width 800)
(def height 600)
(def bounds (rect/rect 0 0 width height))
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-shape [size]
  (let [n (dr/weighted {3 8 4 4 5 1 6 2 7 1 8 1})
        r (poly/circumradius-side-length n size)]
    (poly/regular-n-gon n r)))

(defn vector-set [vertices]
  (set (for [[x y] vertices]
         (f/format [(f/float 3) (f/float 3)] x y))))

(defn tiles-structure? [structure shape]
  (let [overlaps (filter (fn [s] (collide/overlaps? s shape)) structure)
        one-edge (filter
                  (fn [s]
                    (let [isecs (set/intersection (vector-set (g/vertices s))
                                                  (vector-set (g/vertices shape)))
                          n-isecs (count isecs)]
                      ;; FIXME still some false negatives
                      (cond
                        ;; allow an edge to intersect
                        (= n-isecs 2)
                        ;; but ensure no other points overlap
                        (= isecs
                           (vector-set (filter (fn [p] (g/contains-point? s p)) (g/vertices shape)))
                           (vector-set (filter (fn [p] (g/contains-point? shape p)) (g/vertices s))))
                        ;; allow one point to intersect
                        ;; FIXME false positive if edges overlaps but no points
                        (= n-isecs 1)
                        (= isecs
                           (vector-set (filter (fn [p] (g/contains-point? s p)) (g/vertices shape)))
                           (vector-set (filter (fn [p] (g/contains-point? shape p)) (g/vertices s))))
                        :else
                        false)))
                  overlaps)]
    (cond (empty? overlaps)
          true
          (= (count one-edge) (count overlaps))
          true
          :else
          (do (println {:shape shape :overlaps overlaps :one-edge one-edge})
              false))))

(defn gen-shapes [size shape n]
  (reset! defo [])
  (loop [structure [shape]
         faces (set (g/edges shape))
         annotation []]
    (if (or (empty? faces) (>= (count structure) n))
      {:structure structure :annotation annotation :faces faces}
      (let [face (dr/rand-nth (into [] faces))
            [fp fq] face
            mid (tm/mix fp fq 0.5)
            structure-face (g/normal (tm/- fq fp))
            shape (g/rotate (random-shape size) (g/heading (tm/- structure-face)))
            apothem (poly/apothem-side-length (count (g/vertices shape)) size)
            pos (tm/- mid (tm/normalize structure-face apothem))
            shape' (g/center shape pos)
            edges (drop 1 (sort-by (fn [[p q]] (g/dist-squared mid (tm/mix p q 0.5)))
                                   (g/edges shape')))
            inside? (collide/bounded? bounds shape')
            tiles? (tiles-structure? structure shape')]
        (swap! defo conj
               {:shape shape'
                :faces faces
                :face face
                :added [inside? tiles?]})
        (if (and inside? tiles?)
          (recur (conj structure shape')
                 (set/union (disj faces face) (set edges))
                 (into annotation [(gc/circle mid 2.0) (gc/circle pos 1.0)]))
          (recur structure
                 (disj faces face)
                 (conj annotation (vary-meta shape' assoc :stroke "red"))))))))

(defn shapes [{:keys [structure annotation faces]}]
  (conj (concat structure (if (:debug @ui-state) annotation []))
        (csvg/group {:stroke-width 2.0}
          (mapv (fn [[p q]] (gl/line2 p q)) faces))))

(defn scene [generated]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes generated)))

(defn page []
  (let [size (* 0.08 height)
        starting (vary-meta (g/center (random-shape size)
                                      (rv 0.5 0.5))
                            assoc :stroke "#772222")
        generated (gen-shapes size starting 20)]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene generated]]
       [:div.flexcols
        [view-sketch/generate :patterns-shifted]
        [:div.readable-width
         [ctrl/checkbox ui-state "Debug" [:debug]]
         (when (:debug @ui-state)
           [debug/display defo])]]])))

(sketch/definition patterns-shifted
  {:created-at "2024-05-23"
   :tags #{}
   :type :svg}
  (ctrl/mount page))

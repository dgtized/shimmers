(ns shimmers.sketches.morse-patterns
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn poly-divide [gap-cycle len-cycle line]
  (let [{[p q] :points} (g/scale-size line 1.01)
        dist (g/dist p q)
        perp (g/rotate (tm/normalize (tm/- p q) (/ (:width line) 2)) (* 0.25 eq/TAU))
        u0 (tm/+ p perp)
        l0 (tm/- p perp)
        upper (tm/normalize (tm/- (tm/+ q perp) u0) 1.0)
        lower (tm/normalize (tm/- (tm/- q perp) l0))]
    (loop [polys [] a 0 b 20]
      (if (>= b dist)
        polys
        (let [gap (gap-cycle)
              len (len-cycle)]
          (recur (conj polys (gp/polygon2 (tm/+ u0 (tm/* upper a))
                                          (tm/+ u0 (tm/* upper b))
                                          (tm/+ l0 (tm/* lower b))
                                          (tm/+ l0 (tm/* lower a))))
                 (+ b gap)
                 (+ b gap len)))))))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)]
    (mapcat (partial poly-divide (dr/cyclic [5 10]) (dr/cyclic [10 20 30 25]))
            (clip/variable-hatching bounds 0.3 0 120
                                    (dr/cyclic [12 13 14])
                                    (dr/cyclic [4 6 4 15 8])))))

;; FIXME: handle large gaps and overlapping lines
(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (for [[i shape] (map-indexed vector (shapes))]
              (vary-meta shape assoc :key (str "l" i)
                         :stroke-width (:width shape)))))

(sketch/definition morse-patterns
  {:created-at "2021-12-02"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :morse-patterns)
              "sketch-host"))

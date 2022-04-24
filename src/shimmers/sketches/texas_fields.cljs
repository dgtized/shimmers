(ns shimmers.sketches.texas-fields
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn cut-rectangle [cell {[pl ql] :points :as line}]
  (let [edges (g/edges cell)
        isecs (keep (fn [edge]
                      (let [[pe qe] edge
                            isec (isec/intersect-line2-line2? pl ql pe qe)]
                        (when (= (:type isec) :intersect)
                          [edge {:p (:p isec) :line line}]))) edges)]
    isecs))

(comment (cut-rectangle (rect/rect 10) (gl/line2 4 0 8 10)))

(defn make-roads []
  [(gl/line2 (rv 0 (dr/random 0.2 0.8)) (rv 1 (dr/random 0.2 0.8)))
   (gl/line2 (rv (dr/random 0.2 0.8) 0) (rv (dr/random 0.2 0.8) 1))])

(defn make-grid []
  (for [j (tm/norm-range 15)
        i (tm/norm-range 20)]
    (rect/rect (rv i j) (tm/+ (rv i j) (gv/vec2 (/ width 20) (/ height 15))))))

(defn landscape []
  (let [roads (make-roads)
        grid (make-grid)]
    (concat (map (fn [cell] (if-let [isec (some (fn [line] (g/intersect-line cell line)) roads)]
                             (with-meta cell {:fill "hsl(0,50%,50%,10%)"})
                             cell))
                 grid) roads)))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (landscape))))

(sketch/definition texas-fields
  {:created-at "2022-04-24"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :texas-fields)
              "sketch-host"))

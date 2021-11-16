(ns shimmers.sketches.wood-grain
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-line [a b controls scale]
  (let [perpendicular (tm/normalize (g/normal (tm/- b a)) scale)]
    (-> (concat [a]
                (for [t (cs/midsection (tm/norm-range (inc controls)))]
                  (tm/+ (tm/mix a b t)
                        (tm/* perpendicular (dr/random -1 1))))
                [b])
        bezier/auto-spline2
        (g/sample-uniform (* 0.01 height) true)
        gl/linestrip2)))

(defn lines-between [[a b] offsets]
  (for [t offsets]
    (lines/mix-line a b t)))

(defn control-lines [n]
  (concat [(gl/line2 (rv -0.2 0.0) (rv -0.2 1.0))]
          (for [t (dr/var-range n)]
            (lines/simplify-line
             (make-line (rv t 0.0) (rv t 1.0)
                        (dr/rand-nth [2 3 5 6])
                        (/ width (* 4 n)))
             (* 0.0002 width)))
          [(gl/line2 (rv 1.2 0.0) (rv 1.2 1.0))]))

(defn lines [n divisions]
  (mapcat (fn [[a b]]
            (conj
             (lines-between [a b] (cs/midsection (dr/var-range (inc divisions))))
             b))
          (partition 2 1 (control-lines n))))

;; FIXME: handle large gaps and overlapping lines
(defn scene []
  (let [shapes (lines 4 12)]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :stroke-width 1.0}
              (for [[i shape] (map-indexed vector shapes)]
                (vary-meta shape assoc :key (str "l" i))))))

(defn page []
  [:div (scene)])

(defn ui-controls []
  [:div
   [:p.center (view-sketch/generate :wood-grain)]])

(sketch/definition wood-grain
  {:created-at "2021-11-14"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "canvas-host")
  (ctrl/mount ui-controls))

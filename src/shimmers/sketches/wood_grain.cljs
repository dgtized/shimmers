(ns shimmers.sketches.wood-grain
  (:require
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
   [thi.ng.math.core :as tm]
   [shimmers.algorithm.lines :as lines]))

(def width 800)
(def height 600)
(defn r [x y]
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

(defn var-range
  [n]
  {:pre [(pos-int? n)]}
  (let [dt (/ 1.0 (inc n))]
    (sort (concat [0.0]
                  (->> #(dr/gaussian dt (* dt 0.2))
                       (repeatedly n)
                       (reductions +))
                  [1.0]))))

(comment (var-range 1)
         (var-range 2)
         (var-range 5))

(defn lines-between [[a b] n]
  (for [t (cs/midsection (var-range (inc n)))]
    (lines/mix-line a b t)))

(defn random-swap [lines]
  (let [n (count lines)
        i (dr/random-int (dec n))]
    (concat (take (dec i) lines)
            [(nth lines (inc i))
             (nth lines i)]
            (drop (inc i) lines))))

(defn control-lines [n]
  (concat [(gl/line2 (r -0.2 0.0) (r -0.2 1.0))]
          (for [t (var-range n)]
            (lines/simplify-line
             (make-line (r t 0.0) (r t 1.0)
                        (dr/rand-nth [2 3 5 6])
                        (/ width (* 4 n)))
             (* 0.0002 width)))
          [(gl/line2 (r 1.2 0.0) (r 1.2 1.0))]))

(defn lines [n divisions]
  (mapcat (fn [[a b]]
            (conj
             (lines-between [a b] divisions)
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

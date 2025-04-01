(ns shimmers.sketches.iterated-function-system
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn gen-params []
  {:a (- (dr/random-tau) math/PI)
   :b (- (dr/random-tau) math/PI)
   :c (- (dr/random-tau) math/PI)
   :d (- (dr/random-tau) math/PI)})

(defn dejong-ifs [a b c d]
  (fn [[x y]]
    (gv/vec2 (- (math/sin (* a y)) (math/cos (* b x)))
             (- (math/sin (* c x)) (math/cos (* d y))))))

(defn sierpinsky-ifs []
  (fn [[x y]]
    (dr/weighted {(gv/vec2 (/ x 2.0) (/ y 2.0)) 1.0
                  (gv/vec2 (+ (/ x 2.0) 0.5) (/ y 2.0)) 1.0
                  (gv/vec2 (/ x 2.0) (+ (/ y 2.0) 0.5)) 1.0})))

(defn shapes [ifs]
  (for [seed (repeatedly 128 dr/randvec2)
        point (take 128 (iterate ifs seed))]
    (gc/circle (tm/+ (rv 0.5 0.5) (tm/* point (* height 0.225)))
               0.5)))

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "none"
                   :fill "black"}
    (let [ifs (case (:mode @params)
                "dejong"
                (let [{:keys [a b c d]} @params]
                  (dejong-ifs a b c d))
                "sierpinsky"
                (sierpinsky-ifs))]
      (shapes ifs))))

(defn ui-controls [{:keys [params]}]
  [ctrl/container {:class "wide-input"}
   (ctrl/dropdown params "Mode" [:mode]
                  {"DeJong" "dejong"
                   "Sierpinsky" "sierpinsky"})
   (when (= "dejong" (:mode @params))
     [:div
      (ctrl/numeric params "A" [:a] [(- math/PI) math/PI 0.01])
      (ctrl/numeric params "B" [:b] [(- math/PI) math/PI 0.01])
      (ctrl/numeric params "C" [:c] [(- math/PI) math/PI 0.01])
      (ctrl/numeric params "D" [:d] [(- math/PI) math/PI 0.01])])])

(sketch/definition iterated-function-system
  {:created-at "2025-03-31"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (usvg/page (assoc sketch-args
                     :params
                     (ctrl/state (assoc (gen-params)
                                        :mode "dejong"))
                     :explanation ui-controls)
              scene)))

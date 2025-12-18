(ns shimmers.sketches.follow-the-path
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.bias-gain :as mbg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce defo (debug/state {}))

(defn gen-s []
  (dr/weighted {0.5 2.0 1.5 2.0
                0.75 2.0 1.25 2.0
                0.66 2.0 1.33 2.0
                0.33 1.0 1.66 1.0
                (dr/random 0.1 0.9) 1.0
                (dr/random 1.1 1.9) 1.0}))

(defn gen-t []
  (dr/weighted {0.0 1.0 0.25 0.5 0.33 0.5
                0.5 1.0 0.66 0.5 0.75 0.5 1.0 1.0
                (dr/random 0.05 0.95) 2.0}))

(defn bias-sweep [s t]
  (fn [x] (mbg/bias-gain x s t)))

(defn perp [path mag x]
  (let [p (path x)
        dp (g/rotate (tm/normalize (tm/- (path (+ x 0.01)) p) (mag x))
                     (* 0.25 eq/TAU))]
    (gl/line2 (tm/- p dp) (tm/+ p dp))))

(defn gen-path [f1 f2]
  (let [functions
        [{:id "ramp"
          :f (fn [x] (+ 0.5 (* (+ 0.1 (* 0.2 x))
                              (math/sin (+ (* f1 eq/TAU x) (math/sin (* f2 eq/TAU x)))))))
          :weight 1.0}
         {:id "osc"
          :f (fn [x] (+ 0.5 (* 0.3 (math/sin (+ (* f1 eq/TAU x) (math/sin (* f2 eq/TAU x)))))))
          :weight 1.0}]]
    (dr/weighted-by :weight functions)))

(defn shapes []
  (let [s (gen-s)
        t (gen-t)
        f1 (dr/random 0.5 3)
        f2 (dr/random 0.5 4)
        invert (dr/weighted [[identity 2.0] [(fn [x] (- 1.0 x)) 1.0]])
        {:keys [id f]} (gen-path f1 f2)
        path (fn [x] (rv x (f x)))
        magnitude (fn [x] (* (max width height)
                            (math/sin (+ (* 0.33 f2 eq/TAU x) (math/sin (* 0.33 f1 eq/TAU x))))))]
    (swap! defo assoc :id id :s s :t t :f1 f1 :f2 f2)
    (into [#_(gl/linestrip2 (mapv path (tm/norm-range 200)))]
          (mapv (partial perp (comp path invert) magnitude)
                (map (bias-sweep s t)
                     (tm/norm-range 250))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(defn explanation []
  (debug/display defo))

(sketch/definition follow-the-path
  {:created-at "2025-12-15"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))

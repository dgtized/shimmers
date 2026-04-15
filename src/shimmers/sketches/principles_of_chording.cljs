(ns shimmers.sketches.principles-of-chording
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; https://en.wikipedia.org/wiki/Maurer_rose

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn maurer-rose [{:keys [samples n d]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)
        path (for [i (range (inc samples))
                   :let [k (tm/radians (* d i))]]
               (v/+polar center (* radius (math/sin (* n k))) k))]
    (csvg/path (csvg/segmented-path path))))

(defn chorded [{:keys [samples n d]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)]
    (for [i (range (inc samples))
          :let [k (* n i)]]
      (gl/line2 (v/+polar center radius
                          (tm/radians i))
                (v/+polar center radius
                          (+ (tm/radians i) (mod (tm/radians k) d)))))))

(defn modulo-chords [{:keys [samples n]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)]
    (for [i (range (inc samples))]
      (gl/line2 (v/+polar center radius
                          (tm/radians i))
                (v/+polar center radius
                          (mod (tm/radians (* n i )) eq/TAU))))))

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (({:maurer-rose maurer-rose
       :chorded chorded
       :modulo-chords modulo-chords}
      (:method @params))
     @params)))

(defn param-gen []
  (let [method (dr/weighted {:maurer-rose 1.25
                             :chorded 1
                             :modulo-chords 1})]
    (ctrl/state
     (case method
       :maurer-rose
       {:method method
        :samples (dr/rand-nth [360 720])
        :n ((if (dr/chance 0.5) math/round identity)
            (dr/random 2 16))
        :d ((if (dr/chance 0.3) math/round identity)
            (dr/random 2 128))}
       :chorded
       {:method method
        :samples (dr/rand-nth [360 720])
        :n ((if (dr/chance 0.4) math/round identity)
            (min (dr/random 0.5 9)
                 (dr/random 0.5 9)))
        :d (min (dr/random 1.0 eq/TAU)
                (dr/random 1.0 eq/TAU))}
       :modulo-chords
       {:method method
        :samples (dr/rand-nth [360 720])
        :n (dr/weighted {(dr/random 0.1 5) 3
                         (dr/random 0.1 360) 1})}))))

(defn explanation [{:keys [params]}]
  (let [{:keys [samples method]} @params]
    [:div.evencols
     [ctrl/container
      [:div.flexcols [:label "Samples: "] samples]
      [:div.flexcols [:label "Method: "] [:code method]]
      [:div.wide-input
       [ctrl/numeric params "N" [:n] [0.01 360 0.00001]]]
      (when (not= method :modulo-chords)
        [:div.wide-input
         [ctrl/numeric params "D" [:d] [0.01 128 0.00001]]])]]))

(sketch/definition principles-of-chording
  {:created-at "2026-04-14"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (-> sketch-args
       (usvg/with-param-gen param-gen)
       (usvg/with-explanation explanation)
       (usvg/page scene))))

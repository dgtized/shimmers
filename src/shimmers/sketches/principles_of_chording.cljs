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

(defn maurer-rose [{:keys [samples rotation n d pm m]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)
        rot (tm/radians rotation)]
    (->> (for [i (range (inc samples))
               :let [k (tm/radians (* d i))]]
           (v/+polar center
                     (* radius (math/sin (+ rot (* n k)
                                            (* pm (math/sin (tm/radians (* m i)))))))
                     (+ k rot)))
         csvg/segmented-path
         csvg/path)))

(defn chorded [{:keys [samples rotation n d]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)]
    (for [i (range (inc samples))
          :let [base (+ (tm/radians i) (tm/radians rotation))
                k (* n i)]]
      (gl/line2 (v/+polar center radius base)
                (v/+polar center radius
                          (+ base (mod (tm/radians k) d)))))))

(defn modulo-chords [{:keys [samples rotation n]}]
  (let [center (rv 0.5 0.5)
        radius (* 0.49 height)]
    (for [i (range (inc samples))]
      (gl/line2 (v/+polar center radius
                          (+ (tm/radians i) (tm/radians rotation)))
                (v/+polar center radius
                          (mod (tm/radians (+ (* n i) rotation)) eq/TAU))))))

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
      (keyword (:method @params)))
     @params)))

(defn param-gen []
  (let [method (dr/weighted {:maurer-rose 1.25
                             :chorded 1
                             :modulo-chords 1})]
    (->
     (case method
       :maurer-rose
       {:n ((if (dr/chance 0.5) math/round identity)
            (dr/random 2 16))
        :d ((if (dr/chance 0.3) math/round identity)
            (dr/random 2 128))
        :pm (if (dr/chance 0.75) 0.0 1.0)
        :m (if (dr/chance 0.66) 0.0
               (/ (float (dr/random-int 0 64))
                  (float (dr/weighted {1.0 6 2.0 2 3.0 1 4.0 1}))))}
       :chorded
       {:n ((if (dr/chance 0.4) math/round identity)
            (min (dr/random 0.5 9)
                 (dr/random 0.5 9)))
        :d (min (dr/random 1.0 eq/TAU)
                (dr/random 1.0 eq/TAU))}
       :modulo-chords
       {:n (dr/weighted {(dr/random 0.1 5) 3
                         (dr/random 0.1 360) 1})})
     (merge
      {:method method
       :rotation (if (dr/chance 0.1) (dr/random 0 360) 0)
       :samples (dr/rand-nth [360 720])})
     ctrl/state)))

(defn explanation [{:keys [params]}]
  (let [{:keys [method]} @params]
    [:div.evencols.wide-input
     [ctrl/container
      [ctrl/dropdown params "Method" [:method]
       (mapv (fn [n] [n (name n)])
             [:maurer-rose :chorded :modulo-chords])
       {:reader keyword}]
      [ctrl/numeric params "Rotation°" [:rotation] [0 360 0.1]]
      [ctrl/numeric params "Samples" [:samples] [1 5000 1]]
      [ctrl/numeric params "N" [:n] [0.01 360 0.00001]]
      (when (not= method :modulo-chords)
        [ctrl/numeric params "D" [:d] [0.01 128 0.00001]])
      (when (= method :maurer-rose)
        [ctrl/numeric params "Phase M" [:pm] [0.00 16.0 0.001]])
      (when (= method :maurer-rose)
        [ctrl/numeric params "M" [:m] [0.01 64 0.0001]])]]))

(sketch/definition principles-of-chording
  {:created-at "2026-04-14"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (-> sketch-args
       (usvg/with-param-gen param-gen)
       (usvg/with-explanation explanation)
       (usvg/page scene))))

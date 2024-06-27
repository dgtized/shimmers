(ns shimmers.sketches.amplification
  (:require
   [clojure.math :as math]
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn skip-line [a b]
  (let [n (dr/random-int 12 48)
        segment-len (dr/rand-nth [2 3 4])
        gap-spacing (max segment-len (dr/rand-nth [2 3 4]))]
    (->> (tm/norm-range n)
         (drop 1)
         (partition segment-len gap-spacing)
         (mapcat (fn [s]
                   [[:M (tm/mix a b (first s))]
                    [:L (tm/mix a b (last s))]]))
         csvg/path)))

(defn arc-segment [pos r t0 t1]
  (let [src (v/+polar pos r t0)
        dest (v/+polar pos r t1)]
    [[:M src]
     [:A [r r] 0.0
      (if (> (abs (- t1 t0)) math/PI) 1 0)
      (if (> t1 t0) 1 0)
      dest]]))

(defn segmented-circle [pos r]
  (let [n (math/ceil (* 64 (dr/circular-random)))
        gap-spacing (dr/rand-nth [2 3 4])
        base (dr/random-tau)]
    (if (<= n 1)
      (gc/circle pos r)
      (->> (tm/norm-range n)
           (partition 2 gap-spacing)
           (mapcat (fn [[s0 s1]]
                     (arc-segment pos
                                  (* r (dr/gaussian 1.0 0.01))
                                  (+ base (* s0 eq/TAU))
                                  (+ base (* s1 eq/TAU)))))
           csvg/path))))

(defn radial-circle [pos r disp]
  (let [a (gc/circle pos (- r disp))
        b (gc/circle pos (+ r disp))
        n (int (* 2 r (dr/circular-random)))
        base (dr/random-tau)]
    (->> (tm/norm-range n)
         (mapcat (fn [o] [[:M (g/point-at a (+ base o))]
                         [:L (g/point-at b (+ base o))]]))
         (csvg/path))))

(defn sketch-circle [{:keys [p r]}]
  (let [n (math/ceil (* 6 (dr/circular-random)))]
    (map (fn [_]
           (let [p' (tm/+ p (dr/jitter (dr/gaussian 2.0 0.5)))]
             (dr/weighted [[(gc/circle p' r) 5]
                           [(segmented-circle p' r) 3]
                           [(radial-circle p' r (dr/rand-nth [2 3 4 5])) 1]])))
         (range n))))

(defn exp-range [exp n]
  (map #(math/pow % exp) (tm/norm-range n)))

(comment (exp-range 2 10))

(defn make-concentric [{:keys [p r]} offsets]
  (mapcat (fn [o] (sketch-circle (gc/circle p (* r o))))
          (drop 1 ((dr/weighted [[tm/norm-range 2]
                                 [(partial exp-range tm/PHI) 1]
                                 [(partial exp-range (* 2 tm/PHI)) 1]])
                   offsets))))

(defn generate-root+children [bounds]
  (let [center ((dr/weighted [[(fn [] (rv (dr/random 0.25 0.75) (dr/random 0.35 0.65))) 1]
                              [(fn [] (rv 0.5 (dr/gaussian 0.5 0.05))) 1]
                              [(fn [] (rv 0.5 (dr/gaussian 0.33 0.05))) 1]
                              [(fn [] (rv 0.5 (dr/gaussian 0.66 0.05))) 1]]))
        close-edge-point (g/closest-point bounds center)
        edge-dist (g/dist center close-edge-point)
        children (dr/random-int 3 12)]
    [(gc/circle center (* 0.66 edge-dist))
     (keep (fn [t]
             (let [direction (dr/gaussian (+ (* 0.75 eq/TAU) (* eq/TAU t)) 0.2)
                   proj (v/+polar center
                                  (+ (* (- 1 (eq/cos-similarity (tm/- close-edge-point center)
                                                                (v/polar 1 direction)))
                                        (* 0.5 edge-dist))
                                     (* 0.6 edge-dist))
                                  direction)
                   proj-edge-dist (poly/dist-to-closest-point bounds proj)]
               (when (g/contains-point? (g/scale-size bounds 0.9) proj)
                 (gc/circle proj (* 0.4 proj-edge-dist)))))
           (drop 1 (tm/norm-range children)))]))

(defn shapes [bounds]
  (let [[{center :p :as root} children] (generate-root+children bounds)]
    (concat (make-concentric root (dr/rand-nth [5 6 8]))
            (mapcat (fn [{proj :p :as c}]
                      (concat (make-concentric c (dr/rand-nth [3 4 5]))
                              [(skip-line center proj)]))
                    children))))

(defn scene []
  [:div
   [kb/kb-action "alt-s" #(svg-export/download "scene" "amplification")]
   (csvg/svg-timed
     {:id "scene"
      :width width
      :height height
      :stroke "black"
      :fill "none"
      :stroke-width 1.0}
     (shapes (rect/rect 0 0 width height)))])

(sketch/definition amplification
  {:created-at "2023-03-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :amplification)))

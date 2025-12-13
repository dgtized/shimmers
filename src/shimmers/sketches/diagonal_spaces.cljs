(ns shimmers.sketches.diagonal-spaces
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.bias-gain :as mbg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.stair :as ms]
   [shimmers.math.wave :as wave]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn bias-sweep []
  (let [s (dr/weighted {0.5 1.0 1.5 1.0 0.66 1.0 1.33 1.0})
        t (dr/weighted {0.0 1.0 0.25 0.5 0.33 0.5
                        0.5 1.0 0.66 0.5 0.75 0.5 1.0 1.0})]
    (fn [x] (mbg/bias-gain x s t))))

(defn row [a b n slant slant-sweep]
  (mapcat
   (fn [t]
     (let [s (slant-sweep t)]
       [(gl/line2 (rv (- t (* s slant)) a) (rv (+ t (* s slant)) b))]))
   (mapv (dr/weighted {identity 20.0
                       (partial ms/staircase (dr/random-int 3 13)) 1.0
                       (bias-sweep) 4.0})
         (dr/weighted {(tm/norm-range n) 6.0
                       (dr/gaussian-range (/ 1.0 n) (/ 0.2 n) true) 1.0
                       (dr/density-range (/ 0.5 n) (/ 1.5 n) true) 1.0}))))

(defn left-vert-right [x]
  (cond (< x -0.5) -1
        (< -0.5 x 0.5) 0
        (< 0.5 x) 1))

(defn left-right [t]
  (if (< t 0.0) -1 1))

(defn left-vert [t]
  (if (< t 0.0) -1 0))

(defn vert-right [t]
  (if (< t 0.0) 0 1))

(defn shapes []
  (let [rows (dr/weighted {5 1 7 1 9 2 11 2 13 1 15 1})]
    (for [[a b] (partition 2 1 (dr/weighted {(tm/norm-range rows) 1.0
                                             (dr/gaussian-range (/ 1.0 rows) (/ 0.2 rows) true) 1.0}))]
      (let [gap (* 0.05 (- b a))
            ga (+ a gap)
            gb (- b gap)
            slant (* (dr/random-sign) (dr/weighted {0.025 4 0.03 1 0.015 1}))
            n (dr/weighted {100 1 125 0.5 150 1 175 0.5 200 0.5 250 0.5})
            [vert diag] (dr/weighted {[6 1] 4.0 [1 4] 1.0})
            m (dr/rand-nth [left-vert-right left-right left-vert vert-right])
            slant-sweep
            (dr/weighted
             [[(let [p-slant (dr/weighted [[(/ (dr/random 1.0 16.0) n) vert]
                                           [(- 1.0 (/ (dr/random 1.0 16.0) n)) diag]])]
                 (fn [t] (if (and (<= 0.025 t 0.975) (dr/chance p-slant)) 1 0))) 3.0]
              [(let [f (dr/random 0.1 16)
                     k (dr/random 0.5 3.0)
                     p-slant (/ k n)]
                 (fn [t]
                   (if (dr/chance p-slant) (dr/rand-nth [-1 0 1])
                       (m (wave/triangle (/ 1.0 f) t))))) 1.0]])]
        (svg/group {}
                   (gl/line2 (rv 0 ga) (rv 1 ga))
                   (gl/line2 (rv 0 gb) (rv 1 gb))
                   (svg/group {} (row ga gb n slant slant-sweep)))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.8}
    (shapes)))

(defn explanation []
  [:div.readable-width
   "Explorations of textures from repetition with random outliers."])

(sketch/definition diagonal-spaces
  {:created-at "2025-11-29"
   :tags #{:deterministic}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))

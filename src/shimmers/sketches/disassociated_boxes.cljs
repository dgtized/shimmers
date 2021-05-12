(ns shimmers.sketches.disassociated-boxes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.color :as color]
            [shimmers.math.probability :as p]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn shrink [scale shape]
  (geom/scale-size shape scale))

(defn displace [scale shape]
  (geom/translate shape (tm/* (gv/randvec2) (* scale (geom/area shape)))))

(def palettes
  (map color/url->palette
       ["https://artsexperiments.withgoogle.com/artpalette/colors/3a3737-a25547-a19382-c9b9a5-ece7e1"
        "https://artsexperiments.withgoogle.com/artpalette/colors/7085ad-d0d2c8-556895-969796-8fa4c3"
        "https://artsexperiments.withgoogle.com/artpalette/colors/7f2e14-5d503f-e4c111-806d4e"
        "https://artsexperiments.withgoogle.com/artpalette/colors/e9e4dd-9f9f8c-dfd8c8-576945-363f27"]))

(defn colorize [palette shape]
  (assoc shape :color (rand-nth palette)))

(comment (colorize (first palettes) (displace 0.01 (rect/rect 5 5 10 10))))

(def divisions
  {[2 1] 3 [1 2] 3
   [3 1] 3 [1 3] 3
   [4 1] 1 [1 4] 1
   [5 1] 3 [1 5] 3
   [6 1] 1 [1 6] 1
   [7 1] 2 [1 7] 2
   [3 2] 1 [2 3] 1
   [2 2] 1 [3 3] 1})

(defn split-bias
  "Generates divisions and biases the weights towards row splits or column splits."
  [row-bias]
  (reduce-kv (fn [m [r c] v]
               (let [bias (cond (> c r) row-bias
                                (< c r) (/ 1.0 row-bias)
                                :else 1.0)]
                 (assoc m [r c] (* bias v))))
             {}
             divisions))

(defn fib [n]
  (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))

(defn fib-splits [n]
  (let [fibs (drop 2 (fib (+ 2 n)))
        sum (reduce + fibs)]
    (mapv #(/ % sum) (reverse fibs))))

(comment (fib-splits 2)
         (fib-splits 3)
         (reduce + (fib-splits 5)))

(def phi (/ (+ 1 (Math/sqrt 5)) 2))

(defn golden [n]
  (drop 1 (map (fn [i] (/ 1.0 (Math/pow phi i))) (range (inc n)))))

(comment
  (golden 2)
  (golden 3))

(defn subdivide [rect [rows cols]]
  (let [width (geom/width rect)
        height (geom/height rect)
        hstride (/ width cols)
        vstride (/ height rows)
        [x y] (:p rect)]
    (for [i (range cols)
          j (range rows)]
      (rect/rect (+ x (* i hstride))
                 (+ y (* j vstride))
                 hstride vstride))))

(defn disassociate [palette shape]
  ;; bias towards column or row centric splits based on a weighted ratio
  ;; of current shapes width / height
  (let [[rows cols] (->> (/ (+ 2.0 (/ (geom/width shape) (geom/height shape))) 3)
                         split-bias
                         p/weighted)]
    (->> (subdivide shape [rows cols])
         (p/map-random-sample (constantly 0.05)
                              (partial shrink (rand-nth [0.7 0.8 0.9 0.95])))
         (p/map-random-sample (constantly 0.05)
                              (partial displace 0.002))
         (p/map-random-sample (constantly 0.10)
                              (partial colorize palette)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:palette (rand-nth palettes)
   :shapes [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 0.95)]})

(defn update-state [{:keys [shapes palette] :as state}]
  (if (< (count shapes) 1000)
    (update state :shapes
            (partial p/mapcat-random-sample
                     (constantly 0.2)
                     (partial disassociate palette)))
    state))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (doseq [shape shapes]
    (if-let [c (:color shape)]
      (apply q/fill c)
      (q/no-fill))
    (cq/draw-shape (geom/vertices shape))))

(defn ^:export run-sketch []
  ;; 20210505
  (q/defsketch disassociated-boxes
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

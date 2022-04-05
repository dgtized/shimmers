(ns shimmers.sketches.disassociated-boxes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]))

(defn shrink [scales shape]
  (g/scale-size shape (dr/rand-nth scales)))

(defn displace [scale shape]
  (g/translate shape (dr/jitter (* scale (g/area shape)))))

(def palettes
  (map color/url->palette
       ["https://artsexperiments.withgoogle.com/artpalette/colors/3a3737-a25547-a19382-c9b9a5-ece7e1"
        "https://artsexperiments.withgoogle.com/artpalette/colors/7085ad-d0d2c8-556895-969796-8fa4c3"
        "https://artsexperiments.withgoogle.com/artpalette/colors/7f2e14-5d503f-e4c111-806d4e"
        "https://artsexperiments.withgoogle.com/artpalette/colors/e9e4dd-9f9f8c-dfd8c8-576945-363f27"]))

(defn colorize [palette shape]
  (assoc shape :color (dr/rand-nth palette)))

(comment (colorize (first palettes) (displace 0.01 (rect/rect 5 5 10 10))))

(def divisions
  {[2 1] 3 [1 2] 3
   [3 1] 3 [1 3] 3
   [4 1] 1 [1 4] 1
   [5 1] 3 [1 5] 3
   [6 1] 1 [1 6] 1
   [7 1] 2 [1 7] 2
   [3 2] 1 [2 3] 1
   [2 2] 1 [3 3] 1
   [2 5] 1 [5 2] 1
   [3 5] 1 [5 3] 1})

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

(defn fib-splits
  "Subdivide range 0..1 into `n` groups with sizes proportional to
  successive ratios from the fibonnaci sequence.

  (fib-splits 2) => [2/3,1/3]
  (fib-splits 3) => [3/6,2/3,1/6]
  (fib-splits 4) => [5/11,3/11,2/11,1/11]
  "
  [n]
  (let [fibs (drop 2 (fib (+ 2 n)))
        sum (reduce + fibs)]
    (mapv #(/ % sum) (reverse fibs))))

(comment (fib-splits 2)
         (fib-splits 3)
         (fib-splits 4)
         (reduce + (fib-splits 5)))

(defn sum-offsets
  "Takes a set of split offsets, returns a collection of tuples containing the sum
  up to that offset and that offset, ie relative x, x width."
  [coll]
  (reduce (fn [a x] (conj a [(reduce + (map second a)) x]))
          [] coll))

(comment (sum-offsets (fib-splits 2))
         (sum-offsets (fib-splits 3))
         (sum-offsets (fib-splits 4)))

(defn subdivide [rect [rows cols]]
  (let [width (g/width rect)
        height (g/height rect)
        hstride (/ width cols)
        vstride (/ height rows)
        [x y] (:p rect)]
    (for [i (range cols)
          j (range rows)]
      (rect/rect (+ x (* i hstride))
                 (+ y (* j vstride))
                 hstride vstride))))

(defn fib-subdivide [rect [rows cols]]
  (let [width (g/width rect)
        height (g/height rect)
        [hsplits vsplits] (->> [(fib-splits cols) (fib-splits rows)]
                               (dr/map-random-sample (constantly 0.5) reverse))
        [x y] (:p rect)]
    (for [[i-pct i-width] (sum-offsets hsplits)
          [j-pct j-height] (sum-offsets vsplits)]
      (rect/rect (+ x (* i-pct width))
                 (+ y (* j-pct height))
                 (* i-width width)
                 (* j-height height)))))

(defn disassociate [palette shape]
  ;; bias towards column or row centric splits based on a weighted ratio
  ;; of current shapes width / height
  (let [[rows cols] (->> (/ (+ 2.0 (/ (g/width shape) (g/height shape))) 3)
                         split-bias
                         dr/weighted)
        divide-shape (dr/weighted {fib-subdivide 1
                                   subdivide 1})]
    (->> (divide-shape shape [rows cols])
         (dr/map-random-sample (constantly 0.05)
                               (partial shrink [0.7 0.8 0.9 0.95]))
         (dr/map-random-sample (constantly 0.05)
                               (partial displace 0.002))
         (dr/map-random-sample (constantly 0.18)
                               (partial colorize palette)))))

(defn descent [minimum value-fn scale total-area]
  (fn [s] (if (< (/ (g/area s) total-area) 0.0001)
           0.0001
           (max minimum (/ (value-fn s) scale)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [source (cq/screen-rect 0.95)
        area-source (g/area source)
        rules {:constant [(constantly 0.25) 5]
               :by-area [(descent 0.05 g/area area-source area-source) 10]
               :left [(descent 0.1 rect/left (q/width) area-source) 1]
               :right [(descent 0.1 rect/right (q/width) area-source) 1]
               :top [(descent 0.1 rect/top (q/height) area-source) 1]
               :bottom [(descent 0.1 rect/bottom (q/height) area-source) 1]}
        rule-name (dr/weighted (zipmap (keys rules) (map second (vals rules))))]
    (println rule-name)
    {:palette (dr/rand-nth palettes)
     :prob-descent (first (get rules rule-name))
     :shapes [source]}))

(defn update-state [{:keys [shapes prob-descent palette] :as state}]
  (if (< (count shapes) 1200)
    (update state :shapes
            (partial dr/mapcat-random-sample prob-descent
                     (partial disassociate palette)))
    state))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (doseq [shape shapes]
    (if-let [c (:color shape)]
      (apply q/fill c)
      (q/no-fill))
    (cq/draw-polygon shape)))

(sketch/defquil disassociated-boxes
  :created-at "2021-05-05"
  :tags #{:static :deterministic}
  :on-mount #(ctrl/mount (fn [] [:p.center (view-sketch/generate :disassociated-boxes)]))
  :size [1200 900]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

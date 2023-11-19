(ns shimmers.sketches.inside-outside
  (:require
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.flow-fields :as flow]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height (tm/ceil (/ width 1.41)))
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn generate-circles [{[w h] :size :as bounds} R]
  (reduce
   (fn [circles pct]
     (let [radius (* R pct)]
       (pack/circle-pack
        circles
        {:bounds bounds
         :candidates (min (int (/ 20 pct)) 800)
         :gen-circle
         (let [r (dr/random-int (* 0.75 radius) (* 1.25 radius))]
           (fn []
             (let [p (gv/vec2 (dr/random r (- w r)) (dr/random r (- h r)))]
               (gc/circle (tm/+ (:p bounds) p) r))))
         :spacing (max (* 0.005 R) (* 0.1 radius))})))
   []
   [0.2 0.12 0.1 0.08 0.06 0.04 0.02 0.01]))

(defn make-flow [start-fn inside? force-fn lifespan]
  (fn []
    (when-let [path (flow/bidirectional (start-fn) inside? force-fn lifespan)]
      (csvg/path (csvg/segmented-path path)))))

(defn flow-group [circle seed]
  [(csvg/group {}
     (into (if (dr/chance 0.33)
             [circle] [])
           (->> (make-flow
                 (fn [] (rp/inside-circle circle dr/random))
                 (fn [p] (g/contains-point? circle p))
                 (flow/noise-force seed 0.001 4.0)
                 (fn [] (dr/random 128 256)))
                repeatedly
                (keep identity)
                (take (tm/clamp (* 1200 (/ (g/area circle) (* height width)))
                                48 1024)))))])

(defn spaced-flow-group [circle seed]
  (let [force (flow/noise-force seed 0.001 4.0)
        theta (dr/gaussian (+ (g/heading (force (:p circle))) tm/HALF_PI) 0.2)]
    [(csvg/group {}
       (into [circle]
             (->> (make-flow
                   (dr/cyclic
                    (map (fn [t]
                           (let [{:keys [p r]} circle]
                             (tm/mix (v/-polar p r theta) (v/+polar p r theta) t)))
                         (tm/norm-range (int (/ (* 2 (:r circle)) 5)))))
                   (fn [p] (g/contains-point? circle p))
                   force
                   (fn [] (dr/random 128 256)))
                  repeatedly
                  (keep identity)
                  (take (tm/clamp (* 2000 (/ (g/area circle) (* height width)))
                                  48 1024)))))]))

(defn spiral [circle dr dt min-r]
  (->> {:circle circle :t (dr/random-tau) :r (:r circle)}
       (iterate
        (fn [{:keys [circle t r]}]
          (let [r' (* dr r)
                radial-pt (g/point-at circle (/ t eq/TAU))]
            {:circle (gc/circle (v/+polar radial-pt r' (- t Math/PI)) r')
             :t (+ t dt)
             :r r'})))
       (take-while (fn [{:keys [r]}] (> r min-r)))
       (map :circle)))

(defn concentric [circle dr min-r]
  (->> circle
       (iterate
        (fn [{:keys [p r]}]
          (gc/circle p (dr r))))
       (take-while (fn [{:keys [r]}] (> r min-r)))))

(defn restyle [seed circle]
  (let [min-r (* 0.01 (:r circle))]
    (case (dr/weighted {:spiral 3
                        :concentric-limit 1.5
                        :concentric-fixed 2
                        :flow 3
                        :spaced-flow 2
                        :fill 2
                        :drop 1})
      :spiral
      (spiral circle
              (dr/random 0.88 0.96)
              (* (dr/rand-nth [-1 1])
                 eq/TAU
                 (if (dr/chance 0.75)
                   (dr/random 0.05 0.2)
                   (dr/random 0.25 0.5)))
              min-r)
      :flow
      (flow-group circle seed)
      :spaced-flow
      (spaced-flow-group circle seed)
      :concentric-limit
      (concentric circle (let [dr (dr/random 0.80 0.90)]
                           (fn [r] (* r dr)))
                  min-r)
      :concentric-fixed
      (concentric circle (let [dr (* (:r circle) (dr/random 0.05 0.2))]
                           (fn [r] (- r dr)))
                  min-r)
      :drop
      []
      :fill
      [(vary-meta circle assoc :fill "white")])))

(defn shapes [seed]
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.98)
        R (min (g/width bounds) (g/height bounds))
        circles (sort-by :r > (generate-circles bounds R))]
    (into (drop 41 circles)
          (mapcat (partial restyle seed) (take 41 circles)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.33}
                  (shapes (tm/abs (dr/randvec2 1000)))))

(sketch/definition inside-outside
    {:created-at "2023-11-17"
     :tags #{}
     :type :svg}
  (ctrl/mount (view-sketch/static-page scene :inside-outside)))

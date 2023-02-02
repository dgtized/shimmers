(ns shimmers.sketches.chemical-attraction
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def size 32)

(defn add-shape [structure]
  (let [bounds (cq/screen-rect 1.1)
        bonded (g/scale-size (gu/bounding-rect (mapcat g/vertices structure)) 1.02)
        pos (->> #(g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))
                 repeatedly
                 (some (fn [p] (when-not (g/contains-point? bonded p)
                                p))))]
    (triangle/inscribed-equilateral pos size (dr/random-tau))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:structure [(triangle/inscribed-equilateral (cq/rel-vec 0.5 0.5) size (dr/random-tau))]
   :shapes []})

(defn closest-pair [point pairs]
  (apply min-key
         (fn [[p q]]
           (g/dist-squared point (tm/mix p q 0.5)))
         pairs))

(defn outward-face [p q]
  (g/normal (tm/- q p)))

(defn angular-delta [angle target]
  (let [delta (- target angle)]
    (cond (< delta (- Math/PI)) (+ delta eq/TAU)
          (> delta Math/PI) (- delta eq/TAU)
          :else delta)))

(defn angular-acceleration [angle target control angle-vel]
  (let [delta (angular-delta angle target)]
    (- (* control delta)
       (* (* 2 (Math/sqrt control)) angle-vel))))

(defn force-accel [pos target control velocity]
  (let [dir (tm/- target pos)]
    (tm/- (tm/* dir control)
          (tm/* velocity (* 2 (Math/sqrt control))))))

(defn attract-and-bind [{:keys [structure shapes] :as state}]
  (let [faces (mapcat g/edges structure)
        control 0.01
        shapes'
        (for [{:keys [vel angle-vel] :as shape} shapes]
          (let [center (g/centroid shape)
                vel (or vel (gv/vec2))
                angle-vel (or angle-vel 0.0)
                [close-p close-q] (closest-pair center faces)
                mid-structure (tm/mix close-p close-q 0.5)
                [face-p face-q] (closest-pair mid-structure (g/edges shape))
                mid-face (tm/mix face-p face-q 0.5)
                structure-angle (g/heading (outward-face close-p close-q))
                facing-angle (g/heading (tm/- (outward-face face-p face-q)))
                radial-dist (sm/radial-distance facing-angle structure-angle)
                angle-acc (angular-acceleration facing-angle structure-angle control angle-vel)
                acc (force-accel mid-face mid-structure control vel)]
            (if (and (< (g/dist mid-face mid-structure) 1.0) (< radial-dist 0.2))
              (assoc shape :bonded true)
              (-> shape
                  (g/translate (tm/- center))
                  (g/rotate angle-vel)
                  (g/translate (tm/+ center vel))
                  (assoc :angle-vel (+ angle-vel angle-acc)
                         :vel (tm/+ vel acc))
                  (vary-meta assoc :debug {:structure mid-structure :face mid-face})))))]
    (-> state
        (update :structure concat (map (fn [s] (dissoc s :bonded)) (filter :bonded shapes')))
        (assoc :shapes (remove :bonded shapes')))))

(defn update-state [{:keys [structure shapes] :as state}]
  (let [addition
        (if (or (dr/chance 0.95) (> (count structure) 30) (> (count shapes) 0))
          []
          [(add-shape structure)])]
    (-> state
        (update :shapes concat addition)
        attract-and-bind)))

(defn draw [{:keys [structure shapes]}]
  (q/background 1.0)
  (q/stroke 0.0 0.5 0.5)
  (doseq [s structure]
    (qdg/draw s))

  (q/stroke 0.6 0.5 0.5)
  (doseq [s shapes]
    (qdg/draw s)
    (let [{:keys [structure face]} (:debug (meta s))]
      (q/line structure face))))

(sketch/defquil chemical-attraction
  :created-at "2023-02-01"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(ns shimmers.sketches.chemical-attraction
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.control :as control]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def size 16)
(def limit 256)

(defn safe-position [structure]
  (let [bounds (cq/screen-rect 1.1)
        bonded (g/scale-size (gu/bounding-rect (mapcat g/vertices structure)) 1.02)]
    (->> #(g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))
         repeatedly
         (some (fn [p] (when-not (g/contains-point? bonded p)
                        p))))))

(defn create-shape [pos]
  (let [n (dr/weighted {3 3 4 1 5 1})
        R (poly/circumradius-side-length n size)]
    (-> (poly/regular-n-gon n R)
        (g/rotate (dr/random-tau))
        (g/translate pos)
        (assoc :lifespan 0
               :vel (gv/vec2)
               :angle-vel 0.0))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:structure [(create-shape (cq/rel-vec 0.5 0.5))]
   :shapes []})

(defn closest-pair [point pairs]
  (apply min-key
         (fn [[p q]]
           (g/dist-squared point (tm/mix p q 0.5)))
         pairs))

(defn outward-face [p q]
  (g/normal (tm/- q p)))

(defn attract-and-bind [{:keys [structure shapes] :as state}]
  (let [faces (mapcat g/edges structure)
        control 0.1
        shapes'
        (for [{:keys [vel angle-vel lifespan] :as shape} shapes]
          (let [center (g/centroid shape)
                [close-p close-q] (closest-pair center faces)
                mid-structure (tm/mix close-p close-q 0.5)
                [face-p face-q] (closest-pair mid-structure (g/edges shape))
                mid-face (tm/mix face-p face-q 0.5)
                structure-angle (g/heading (outward-face close-p close-q))
                facing-angle (g/heading (tm/- (outward-face face-p face-q)))
                radial-dist (sm/radial-distance facing-angle structure-angle)
                angle-acc (control/angular-acceleration facing-angle structure-angle control angle-vel)
                acc (control/force-accel mid-face mid-structure control vel)
                close-to-bond? (and (< (g/dist mid-face mid-structure) 0.5) (< radial-dist 0.1))
                ;; TODO: actually do collision to avoid internal shapes?
                bonding? (and close-to-bond? (not-any? (fn [s] (g/contains-point? s center)) structure))

                jitter (dr/chance (tm/smoothstep* 50 200 (mod lifespan 180)))
                angle-jitter (* (dr/random (- angle-vel) angle-vel) (if jitter 2.0 0.1))
                vel-jitter (dr/randvec2 (cond
                                          (and close-to-bond? (not bonding?))
                                          32.0
                                          jitter 8.0
                                          :else 0.1))]
            (if bonding?
              (assoc shape :bonded true)
              (-> shape
                  (g/translate (tm/- center))
                  (g/rotate (+ angle-vel angle-jitter))
                  (g/translate (tm/+ center (tm/+ vel vel-jitter)))
                  (assoc :angle-vel (+ angle-vel angle-acc)
                         :vel (tm/+ vel acc)
                         :lifespan (if (and jitter (dr/chance 0.3)) 0.0 (inc lifespan)))
                  (vary-meta assoc :debug {:structure mid-structure :face mid-face})))))]
    (-> state
        (update :structure concat (filter :bonded shapes'))
        (assoc :shapes (remove :bonded shapes')))))

(defn update-state [{:keys [structure shapes] :as state}]
  (let [addition
        (if (or (> (count structure) limit) (> (count shapes) 3) (dr/chance 0.75))
          []
          [(create-shape (safe-position structure))])]
    (-> state
        (update :shapes concat addition)
        attract-and-bind)))

(defn draw [{:keys [structure shapes]}]
  (q/background 1.0)
  (q/stroke 0.0)
  (doseq [s structure]
    (qdg/draw s))

  (q/stroke 0.0 0.5 0.5)
  (doseq [s shapes]
    (qdg/draw s)
    (let [{:keys [structure face]} (:debug (meta s))]
      (q/line structure face)))

  (when (and (> (count structure) limit) (empty? shapes))
    (q/no-loop)))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition chemical-attraction
  {:created-at "2023-02-01"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

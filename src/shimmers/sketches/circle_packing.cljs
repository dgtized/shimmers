(ns shimmers.sketches.circle-packing
  "Inspired by https://thecodingtrain.com/CodingChallenges/050.1-circlepackinganimated.html"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

;; TODO: represent circles as polygons and grow individual points until it
;; intersects a neighbor and only mark the individual vertex as "done".
;; Scott says this should be called "cell-munging"

(defn random-color []
  [(dr/random)
   (dr/random 0.15 0.9)
   (dr/random 0.25 0.8)
   (dr/random 0.25 0.8)])

(defn color-mix [c1 c2]
  (if c2
    (let [[r1 r2] [(:r c1) (:r c2)]
          ;; proportional radius, ie if equal size use 0.5, otherwise weight
          ;; interpolation by color of larger radius
          t (/ r2 (+ r1 r2))
          mixed (color/jitter (color/mixer (:color c1) (:color c2) t)
                              #(* 0.05 (dr/gaussian 0 1)))]
      mixed)
    (:color c1)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:quadtree (saq/circletree (cq/screen-rect))
   :boundary (cq/screen-rect)
   :radius 3
   :scale 1.025
   :circles []})

(defn grow-circles [{:keys [boundary quadtree circles scale] :as state}]
  (loop [tree quadtree processing circles result []]
    (let [[circle & remaining] processing]
      (cond (empty? processing)
            (assoc state
                   :circles result
                   :quadtree tree)
            (:done circle)
            (recur tree
                   remaining
                   (conj result circle))
            :else
            (let [growth (assoc (g/scale-size circle scale) :color (:color circle))
                  near (saq/closest-circle (g/delete-point tree (:p circle)) growth)]
              (if (and (geometry/contains-circle? boundary growth)
                       (and near (> (saq/circle-overlap growth near) 0)))
                (recur (pack/replace-circle-at tree growth)
                       remaining
                       (conj result growth))
                (let [done (assoc circle :done true
                                  :color (color-mix circle near))]
                  (recur (pack/replace-circle-at tree done)
                         remaining
                         (conj result done)))))))))

;; TODO performance from sampling cost?
(defn fresh-circles [state n]
  (let [{:keys [boundary quadtree radius]} state
        r radius
        gen-circle
        (fn [] (-> (gv/vec2 (dr/random r (- (q/width) r))
                           (dr/random  r (- (q/height) r)))
                  (gc/circle r)
                  (assoc :color (random-color))))
        rules {:bounds boundary :gen-circle gen-circle}
        [additions' tree'] (pack/pack-candidates quadtree n rules)]
    (-> state
        (update :circles into additions')
        (assoc :quadtree tree'))))

;; Re-enable growth of nearby circles after cull?
(defn cull-circles [{:keys [circles quadtree] :as state} p]
  (let [culled (dr/random-sample p circles)]
    (assoc state :circles (remove (set culled) circles)
           :quadtree (reduce g/delete-point quadtree (map :p culled)))))

(defn update-state [state]
  (-> state
      grow-circles
      (fresh-circles (dr/random-int 3))
      (cull-circles 0.00002)))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.5)
  (doseq [{:keys [color done] :as c} circles]
    (apply q/fill color)
    (if done
      (q/no-stroke)
      (q/stroke 0 0 0 1.0))
    (cq/circle c)))

(sketch/defquil circle-packing
  :created-at "2021-03-10"
  :size [900 600]
  :tags #{:deterministic}
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

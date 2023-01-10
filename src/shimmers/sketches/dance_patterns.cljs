(ns shimmers.sketches.dance-patterns
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def directions
  (vec (for [x [-1 0 1]
             y [-1 0 1]
             :when (not= x y 0)]
         (gv/vec2 x y))))

(defn legal-moves [size pos]
  (for [dir directions
        :let [pos' (tm/+ pos dir)
              [x y] pos']
        :when (and (<= 0 x (dec size))
                   (<= 0 y (dec size)))]
    dir))

(defn make-action [size {:keys [position]} t]
  (let [move (dr/rand-nth (legal-moves size position))]
    {:move (tm/+ position move)
     :t0 t
     :t1 (+ t (inc (dr/random-int 8)))}))

(defn make-cell [pos]
  {:position pos
   :actions []})

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (-> (rect/rect (q/height))
                   (g/center (cq/rel-vec 0.5 0.5))
                   (g/scale-size 0.9))
        size 7]
    {:bounds bounds
     :size size
     :t 0
     :cells (g/subdivide bounds {:rows size :cols size})
     :actors [(make-cell (gv/vec2 (int (/ size 2)) (int (/ size 2))))]}))

(defn update-actors [actors t size]
  (mapcat (fn [{:keys [actions] :as actor}]
            [(if (empty? actions)
               (update actor :actions conj (make-action size actor t))
               (let [{:keys [move t1]} (first actions)]
                 (if (>= t t1)
                   (-> actor
                       (assoc :position move)
                       (update :actions rest))
                   actor)))])
          actors))

(defn update-state [{:keys [t size] :as state}]
  (-> state
      (update :t + 0.05)
      (update :actors update-actors t size)))

(defn draw [{:keys [bounds cells actors t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 1.0)
  (cq/draw-polygon bounds)
  (doseq [cell cells]
    (cq/draw-polygon cell))

  (q/stroke-weight 2.0)
  (doseq [{:keys [position actions]} actors]
    (let [pos (if (empty? actions)
                position
                (let [{:keys [move t0 t1]} (first actions)]
                  (tm/mix position move (/ (- t t0) (- t1 t0)))))]
      (cq/draw-polygon (g/translate (first cells) (tm/* pos (g/width (first cells))))))))

(sketch/defquil dance-patterns
  :created-at "2023-01-10"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

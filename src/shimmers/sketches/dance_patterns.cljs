(ns shimmers.sketches.dance-patterns
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wave :as wave]
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
    pos'))

(defn empty-spaces [size actors actor]
  (let [current (set (map :position actors))
        next (set (keep (comp :move first :actions) actors))]
    (->> actor
         :position
         (legal-moves size)
         (remove (set/union current next)))))

(defn action-wait [{:keys [actor t]}]
  {:type :wait
   :move (:position actor)
   :t0 t
   :t1 (+ t (inc (dr/random-int 4)))})

(defn action-slide [{:keys [size actors actor t] :as action-state}]
  (let [moves (empty-spaces size actors actor)]
    (if-let [move (and (seq moves) (dr/rand-nth moves))]
      {:type (if (dr/chance 0.5) :rotate :slide)
       :move move
       :t0 t
       :t1 (+ t (inc (dr/random-int 4)))}
      (action-wait action-state))))

(defn action-duplicate [{:keys [size actors actor t] :as action-state}]
  (if (> (count (empty-spaces size actors actor)) 5)
    {:type :duplicate
     :move (:position actor)
     :t0 t
     :t1 (+ t (inc (dr/random-int 4)))}
    (action-wait action-state)))

(defn make-action [size actors actor t]
  (let [max-count (* 0.125 size size)
        n (count actors)
        actions
        {action-wait 3
         action-slide 2
         action-duplicate (if (< n max-count) (/ (- max-count n) max-count) 0)}]
    ((dr/weighted actions) {:size size :actors actors :actor actor :t t})))

(defn make-cell [pos]
  {:position pos
   :actions []})

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (-> (rect/rect (q/height))
                   (g/center (cq/rel-vec 0.5 0.5))
                   (g/scale-size 0.9))
        size 47]
    {:bounds bounds
     :size size
     :last-side 10
     :side 10
     :t 0
     :actors [(make-cell (gv/vec2 (int (/ size 2)) (int (/ size 2))))]}))

(defn add-action [actor action]
  (update actor :actions conj action))

(defn update-actors [actors t size]
  (reduce (fn [actors' {:keys [actions] :as actor}]
            (if (empty? actions)
              (let [action (make-action size (into actors actors') actor t)]
                (conj actors' (add-action actor action)))
              (let [{:keys [type move t1]} (peek actions)]
                (if (>= t t1)
                  (let [actor' (update actor :actions pop)]
                    (if (= type :duplicate)
                      (conj actors' actor' actor')
                      (conj actors' (assoc actor' :position move))))
                  (conj actors' actor)))))
          []
          actors))

(defn scaled-side [{[w h] :size} size actors]
  (let [min-x (apply min (map (comp :x :position) actors))
        max-x (apply max (map (comp :x :position) actors))
        min-y (apply min (map (comp :y :position) actors))
        max-y (apply max (map (comp :y :position) actors))]
    (min (/ w (min size (- (+ max-x 3) (- min-x 3))))
         (/ h (min size (- (+ max-y 3) (- min-y 3)))))))

(defn update-state [{:keys [actors bounds t size last-side side] :as state}]
  (-> state
      (update :t + (/ 1 32))
      (assoc :last-side side
             :side (tm/mix* last-side (scaled-side bounds size actors) 0.05))
      (update :actors update-actors t size)))

(defn rotation-corner [dir]
  (case dir
    [1 0] (gv/vec2 0.5 0.5)
    [-1 0] (gv/vec2 -0.5 -0.5)
    [0 1] (gv/vec2 -0.5 0.5)
    [0 -1] (gv/vec2 0.5 -0.5)
    [1 1] (gv/vec2 0.5 0.5)
    [-1 -1] (gv/vec2 -0.5 -0.5)
    [-1 1] (gv/vec2 -0.5 0.5)
    [1 -1] (gv/vec2 0.5 -0.5)
    (gv/vec2 0 0)))

(defn render-cell [base side size]
  (fn [cell weight position]
    (q/stroke-weight weight)
    (-> cell
        (g/translate (tm/+ base (tm/* (tm/- position (gv/vec2 (/ size 2) (/ size 2)))
                                      side)))
        cq/draw-polygon)))

;; TODO: add size scaling to zoom out as the actor quantity increases?
(defn draw [{:keys [side size actors t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)

  (q/stroke 0.0)
  (q/translate (cq/rel-vec 0.5 0.5))
  (let [draw-cell (render-cell (tm/* (gv/vec2 0.5 0.5) side) side size)
        cell (g/center (rect/rect side))]
    (doseq [{:keys [position actions]} actors]
      (if (empty? actions)
        (draw-cell cell 2.0 position)
        (let [{:keys [type move t0 t1]} (peek actions)
              v (/ (- t t0) (- t1 t0))]
          (case type
            :rotate
            (let [dir (tm/- move position)
                  diagonal (case (apply + (map abs dir))
                             1 false
                             2 true)
                  corner (rotation-corner dir)]
              (-> cell
                  (g/translate (tm/* corner side))
                  (g/rotate (if diagonal
                              (- eq/TAU (* (- 1 v) 0.5 eq/TAU))
                              (+ (* 0.75 eq/TAU) (* (- 1 v) 0.75 eq/TAU))))
                  (draw-cell (- 2.0 (* 1.25 (wave/triangle01 1 v)))
                             (tm/+ corner position))))
            :slide
            (draw-cell cell
                       (- 2.0 (* 1.25 (wave/triangle01 1 v)))
                       (tm/mix position move v))
            :wait
            (draw-cell cell 2.0 (tm/mix position move v))
            :duplicate
            (draw-cell cell 3.0 position)))))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition dance-patterns
  {:created-at "2023-01-10"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

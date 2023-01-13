(ns shimmers.sketches.dance-patterns
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
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

(defn action-wait [{:keys [actor t]}]
  {:type :wait
   :move (:position actor)
   :t0 t
   :t1 (+ t (inc (dr/random-int 4)))})

(defn action-slide [{:keys [size actors actor t] :as action-state}]
  (let [current (set (map :position actors))
        next (set (keep (comp :move first :actions) actors))
        moves (->> actor
                   :position
                   (legal-moves size)
                   (remove (set/union current next)))]
    (if (seq moves)
      {:type (if (dr/chance 0.5) :rotate :slide)
       :move (dr/rand-nth moves)
       :t0 t
       :t1 (+ t (inc (dr/random-int 4)))}
      (action-wait action-state))))

(defn action-duplicate [{:keys [actor t]}]
  {:type :duplicate
   :move (:position actor)
   :t0 t
   :t1 (+ t (inc (dr/random-int 4)))})

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
     :t 0
     :actors [(make-cell (gv/vec2 (int (/ size 2)) (int (/ size 2))))]}))

(defn update-actors [actors t size]
  (reduce (fn [actors' {:keys [actions] :as actor}]
            (if (empty? actions)
              (conj actors'
                    (update actor :actions conj
                            (make-action size (into actors actors') actor t)))
              (let [{:keys [type move t1]} (peek actions)]
                (if (>= t t1)
                  (let [actor' (update actor :actions pop)]
                    (if (= type :duplicate)
                      (conj actors' actor' actor')
                      (conj actors' (assoc actor' :position move))))
                  (conj actors' actor)))))
          []
          actors))

(defn update-state [{:keys [t size] :as state}]
  (-> state
      (update :t + (/ 1 32))
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

(defn draw [{:keys [bounds size actors t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)

  (q/stroke-weight 2.0)
  (q/stroke 0.0)
  (let [{base :p [w _] :size} bounds
        side (/ w size)
        base (tm/+ base (tm/* (gv/vec2 0.5 0.5) side))
        cell (g/center (rect/rect side))]
    (doseq [{:keys [position actions]} actors]
      (cq/draw-polygon
       (if (empty? actions)
         (g/translate cell (tm/+ base (tm/* position side)))
         (let [{:keys [type move t0 t1]} (peek actions)
               v (/ (- t t0) (- t1 t0))]
           (if (= type :rotate)
             (let [dir (tm/- move position)
                   diagonal (case (apply + (map abs dir))
                              1 false
                              2 true)
                   corner (rotation-corner dir)]
               (-> cell
                   (g/translate (tm/* corner side))
                   (g/rotate (if diagonal
                               (- eq/TAU (* (- 1 v) (* 0.5 eq/TAU)))
                               (+ (* 0.75 eq/TAU) (* (- 1 v) 0.75 eq/TAU))))
                   (g/translate (tm/+ base (tm/* (tm/+ corner position) side)))))
             (g/translate cell (tm/+ base (tm/* (tm/mix position move v) side))))))))))

(sketch/defquil dance-patterns
  :created-at "2023-01-10"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

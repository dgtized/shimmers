(ns shimmers.sketches.dance-patterns
  (:require
   [clojure.set :as set]
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
    pos'))

(defn action-wait [_size _actors {:keys [position]} t]
  {:type :wait
   :move position
   :t0 t
   :t1 (+ t (inc (dr/random-int 4)))})

(defn action-slide [size actors {:keys [position] :as actor} t]
  (let [current (set (map :position actors))
        next (set (keep (comp :move first :actions) actors))
        moves (->> position
                   (legal-moves size)
                   (remove (set/union current next)))]
    (if (seq moves)
      {:type :slide
       :move (dr/rand-nth moves)
       :t0 t
       :t1 (+ t (inc (dr/random-int 4)))}
      (action-wait size actors actor t))))

(defn make-action [size actors actor t]
  ((dr/weighted
    {action-wait 2
     action-slide 1})
   size actors actor t))

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
     :actors [(make-cell (gv/vec2 (int (/ size 2)) (int (/ size 2))))]}))

(defn update-actors [actors t size]
  (mapcat (fn [{:keys [actions] :as actor}]
            (if (empty? actions)
              (let [actor' (update actor :actions conj (make-action size actors actor t))]
                (if (and (< (count actors) 21) (dr/chance 0.33))
                  [actor'
                   (update actor :actions conj (action-wait size actors actor t))]
                  [actor']))
              (let [{:keys [move t1]} (first actions)]
                (if (>= t t1)
                  [(-> actor
                       (assoc :position move)
                       (update :actions rest))]
                  [actor]))))
          actors))

(defn update-state [{:keys [t size] :as state}]
  (-> state
      (update :t + (/ 1 32))
      (update :actors update-actors t size)))

(defn draw [{:keys [bounds size actors t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)

  (let [{[x y] :p [w _] :size} bounds
        side (/ w size)
        cell (rect/rect x y side side)]
    (q/stroke-weight 2.0)
    (doseq [{:keys [position actions]} actors]
      (let [pos (if (empty? actions)
                  position
                  (let [{:keys [move t0 t1]} (first actions)]
                    (tm/mix position move (/ (- t t0) (- t1 t0)))))]
        (cq/draw-polygon (g/translate cell (tm/* pos side)))))))

(sketch/defquil dance-patterns
  :created-at "2023-01-10"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

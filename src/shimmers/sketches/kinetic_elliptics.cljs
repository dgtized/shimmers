(ns shimmers.sketches.kinetic-elliptics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.common.quil :as cq]))

(defn fixed-behavior []
  (fn [parent-pos _t] parent-pos))

;; fixed angle to global
;; TODO: add fixed angle relative to parent
(defn fixed-angle [r angle]
  (fn [parent-pos _t]
    (v/+polar parent-pos r angle)))

(defn orbit-behavior [r period phase]
  (let [dtheta (/ eq/TAU period)]
    (fn [parent-pos t]
      (v/+polar parent-pos r (+ (* dtheta t) phase)))))

(defn pendulum-behavior [r t0 t1 period phase]
  (let [t1 (if (< t1 t0) (+ t1 eq/TAU) t1)
        dtheta (/ (* 2 (- t1 t0)) period)]
    (fn [parent-pos t]
      (let [cyclic-t (eq/unit-sin (+ (- (* dtheta t) (/ eq/TAU 4)) phase))]
        (v/+polar parent-pos r (tm/mix* t0 t1 cyclic-t))))))

(defrecord Element [behavior color children])

(defn random-behavior [base-r]
  ((dr/weighted
    [[(fn [] (fixed-angle (* base-r (dr/random 0.2 1.2))
                         (* eq/TAU (dr/rand-nth (butlast (tm/norm-range 8))))))
      1.0]
     [(fn [] (orbit-behavior (* base-r (dr/random 0.2 1.2))
                            (dr/random 6 24)
                            (dr/random-tau)))
      3.0]
     [(fn [] (pendulum-behavior (* base-r (dr/random 0.25 1.25))
                               (dr/random-tau) (dr/random-tau)
                               (dr/random 6 24)
                               (dr/random-tau)))
      3.0]])))

(defn create-elements [base-r n]
  (->Element (random-behavior base-r)
             [0.0 (/ 1.5 (- 6 n))]
             (if (> n 0)
               (repeatedly (dr/weighted {0 (if (> n 2) 0 (/ 1.0 n))
                                         1 4
                                         2 2
                                         3 1
                                         4 0.5})
                           #(create-elements base-r (dec n)))
               [])))

(defn plot-elements
  [origin {:keys [behavior color children] :as element} t]
  (lazy-seq
   (when element
     (let [position (behavior origin t)]
       (cons [origin position color]
             (mapcat (fn [child] (plot-elements position child t))
                     children))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:origin (cq/rel-vec 0.5 0.5)
   :root (assoc (create-elements (cq/rel-h 0.15) 4)
                :behavior (fixed-behavior))
   :t 0.0})

(defn update-state [state]
  (assoc state :t (/ (q/millis) 1000.0)))

(defn draw [{:keys [origin root t]}]
  (q/background 1.0 1.0)
  (q/no-stroke)
  (q/fill 0.0 1.0)
  (cq/circle origin 2.0)
  (doseq [group (apply map vector
                       (map (partial plot-elements origin root)
                            (map + (range 0.0 0.1 (/ 0.1 4)) (repeat t))))]
    (doseq [[parent element color] group]
      (q/no-stroke)
      (cq/circle element 2.0)
      (q/stroke-weight 2.0)
      (apply q/stroke color)
      (q/line parent element))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p.readable-width
    "Randomly generate a tree structure with each node either orbiting or swinging on a pendulum between two random angles relative to it's parent."]])

(sketch/definition kinetic-elliptics
  {:created-at "2023-09-15"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

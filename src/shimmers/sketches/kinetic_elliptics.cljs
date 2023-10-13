(ns shimmers.sketches.kinetic-elliptics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.math.wave :as wave]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def max-depth 4)

(defn heading [{:keys [position origin]}]
  (g/heading (tm/- position origin)))

(defn radius-repeats [repeats phase dtheta t]
  (+ 1 (* 0.25 (Math/sin (+ phase (* repeats dtheta t))))))

(defn cyclic [dtheta phase t]
  (eq/unit-sin (+ (- (* dtheta t) (/ eq/TAU 4)) phase)))

;; TODO: datify these behaviors instead of using functions
(defn fixed-behavior []
  {:pos
   (fn [{:keys [position]} _t] position)})

;; fixed angle to global
(defn fixed-angle [r global-angle]
  {:pos
   (fn [{:keys [position]} _t]
     (v/+polar position r global-angle))})

(defn relative-angle [r rel-angle]
  {:pos
   (fn [{:keys [position] :as parent} _t]
     (v/+polar position r (+ (heading parent)
                             rel-angle)))})

(defn orbit-behavior [r period phase]
  (let [dtheta (/ eq/TAU period)]
    {:pos
     (fn [{:keys [position]} t]
       (v/+polar position r (+ (* dtheta t) phase)))
     :draw
     (fn [[x y] _element _t]
       (q/no-fill)
       (q/stroke-weight 0.2)
       (cq/circle x y r))}))

(defn smooth-stepper [steps e0 e1 t]
  (let [t' (/ t steps)]
    (* steps (+ (Math/floor t')
                (tm/smoothstep* e0 e1 (- t' (Math/floor t')))))))

(comment
  (map (fn [t] (smooth-stepper (/ 6 2) 0.33 0.66 t))
       (range 0 10 0.1)))

(defn clock-behavior [r period steps phase]
  (let [dtheta (/ eq/TAU period)]
    {:pos
     (fn [{:keys [position]} t]
       (let [t' (smooth-stepper (/ period steps) 0.33 0.66 t)]
         (v/+polar position r (+ (* dtheta t') phase))))
     :draw
     (fn [pos _element _t]
       (q/no-fill)
       (q/stroke-weight 0.66)
       (doseq [t (range 0 1 (/ 1.0 steps))]
         (cq/circle (v/+polar pos r (+ (* eq/TAU t) phase)) 1.0)))}))

(defn orbit-r-behavior [base-r repeats period phase]
  (let [dtheta (/ eq/TAU period)]
    {:pos
     (fn [{:keys [position]} t]
       (let [radius (* base-r (radius-repeats repeats phase dtheta t))]
         (v/+polar position radius (+ (* dtheta t) phase))))}))

(defn pendulum-behavior [r t0 t1 period phase]
  (let [t1 (if (< t1 t0) (+ t1 eq/TAU) t1)
        dtheta (/ (* 2 (- t1 t0)) period)]
    {:pos
     (fn [{:keys [position]} t]
       (let [cyclic-t (eq/unit-sin (+ (- (* dtheta t) (/ eq/TAU 4)) phase))]
         (v/+polar position r (tm/mix* t0 t1 cyclic-t))))
     :draw
     (fn [[x y] _element _t]
       (q/no-fill)
       (q/stroke-weight 0.2)
       (q/arc x y r r t0 t1))}))

;; FIXME: at the top of the cycle sometimes tries to go a little too far?
;; something about boundary conditions on triangle01 or smooth-stepper?
(defn pendulum-step-behavior [r t0 t1 period steps phase]
  (let [t1 (if (< t1 t0) (+ t1 eq/TAU) t1)]
    {:pos
     (fn [{:keys [position]} t]
       (let [t' (smooth-stepper (/ period steps) 0.33 0.66 t)
             theta (tm/mix* t0 t1 (wave/triangle01 period t'))]
         (v/+polar position r (+ theta phase))))
     :draw
     (fn [pos _element _t]
       (q/no-fill)
       (q/stroke-weight 0.66)
       (doseq [theta (range t0 t1 (/ (- t1 t0) (/ steps 2)))]
         (cq/circle (v/+polar pos r (+ theta phase)) 1.0)))}))

(defn pendulum-r-behavior [base-r repeats t0 t1 period phase]
  (let [t1 (if (< t1 t0) (+ t1 eq/TAU) t1)
        dtheta (/ (* 2 (- t1 t0)) period)]
    {:pos
     (fn [{:keys [position]} t]
       (let [cyclic-t (cyclic dtheta phase t)
             dist (* base-r (radius-repeats repeats phase dtheta t))]
         (v/+polar position dist (tm/mix* t0 t1 cyclic-t))))}))

(defn relative-pendulum-behavior [r t0 t1 period phase]
  (let [t1 (if (< t1 t0) (+ t1 eq/TAU) t1)
        dtheta (/ (* 2 (- t1 t0)) period)]
    {:pos
     (fn [{:keys [position] :as parent} t]
       (let [cyclic-t (cyclic dtheta phase t)]
         (v/+polar position r (+ (heading parent)
                                 (tm/mix* t0 t1 cyclic-t)))))}))

(defn relative-pendulum-r-behavior [base-r repeats t0 t1 period phase]
  (let [t1 (if (< t1 t0) (+ t1 eq/TAU) t1)
        dtheta (/ (* 2 (- t1 t0)) period)]
    {:pos
     (fn [{:keys [position] :as parent} t]
       (let [cyclic-t (cyclic dtheta phase t)]
         (v/+polar position
                   (* base-r (radius-repeats repeats phase dtheta t))
                   (+ (heading parent)
                      (tm/mix* t0 t1 cyclic-t)))))}))

;; TODO: implement parallel or roller which is a pair of elements. first element
;; ends and then it's child starts a new draw in parallel / offset from the
;; original line.
;; (defn parallel [displace]
;;   (fn [{:keys [position origin]} t]))
;; Maybe worth converting behavior to data before adding multi-stage elements?

(defrecord Element [behavior color children])

(defn random-period [depth]
  (* (Math/pow 1.1 depth)(dr/random 6 30)))

(defn random-behavior [radial-length depth]
  ((dr/weighted
    [[(fn [] (fixed-angle radial-length
                         (* eq/TAU (dr/rand-nth (butlast (tm/norm-range 8))))))
      1.0]
     [(fn [] (relative-angle radial-length
                            (- (* eq/TAU (dr/rand-nth (butlast (tm/norm-range 8))))
                               Math/PI)))
      1.0]
     [(fn [] (clock-behavior radial-length
                            (* (dr/weighted {-1 1 1 1})
                               (* 1.5 (random-period depth)))
                            (dr/random-int 2 32)
                            (dr/random-tau)))
      1.0]
     [(fn [] (orbit-behavior radial-length
                            (* (dr/weighted {-1 1 1 1})
                               (random-period depth))
                            (dr/random-tau)))
      3.0]
     [(fn [] (orbit-r-behavior (* radial-length (dr/random 0.2 1.2))
                              (dr/random-int 1 8)
                              (* (dr/weighted {-1 1 1 1})
                                 (random-period depth))
                              (dr/random-tau)))
      1.5]
     [(fn [] (pendulum-behavior
             radial-length
             (dr/random-tau) (dr/random-tau)
             (random-period depth)
             (dr/random-tau)))
      3.0]
     [(fn [] (pendulum-step-behavior
             radial-length
             (dr/random-tau) (dr/random-tau)
             (random-period depth)
             (dr/random-int 2 12)
             (dr/random-tau)))
      2.0]
     [(fn [] (pendulum-r-behavior
             radial-length
             (dr/random-int 1 8)
             (dr/random-tau) (dr/random-tau)
             (random-period depth)
             (dr/random-tau)))
      1.0]
     [(fn [] (relative-pendulum-behavior
             radial-length
             (- (dr/random-tau)) (dr/random-tau)
             (random-period depth)
             (dr/random-tau)))
      1.5]
     [(fn [] (relative-pendulum-r-behavior
             radial-length
             (dr/random-int 1 8)
             (- (dr/random-tau)) (dr/random-tau)
             (random-period depth)
             (dr/random-tau)))
      1.0]])))

(defn random-element [base-r depth]
  (let [len (* (tm/clamp (/ (dr/gaussian max-depth 0.66)
                            (* 1.5 (inc depth)))
                         0.5
                         3.0)
               base-r)]
    (->Element (random-behavior len depth)
               [0.0 (/ 1.5 (inc depth))]
               [])))

(defn create-elements [base-r depth]
  (assoc (random-element base-r depth) :children
         (if (<= depth max-depth)
           (repeatedly (dr/weighted {0 (if (< depth 2) 0 depth)
                                     1 4
                                     2 2
                                     3 1
                                     4 0.5})
                       #(create-elements base-r (inc depth)))
           [])))

(defn plot-elements
  [parent {:keys [behavior color children] :as element} t]
  (lazy-seq
   (when element
     (let [position ((:pos behavior) parent t)
           origin (:position parent)
           self {:position position :origin origin}]
       (cons [origin position (:draw behavior) color t]
             (mapcat (fn [child] (plot-elements self child t))
                     children))))))

(defn build-tree [base-r]
  (let [tree (assoc (create-elements base-r 0)
                    :behavior (fixed-behavior))
        size (count (plot-elements {:position (gv/vec2) :angle 0} tree 0))]
    (if (< 8 size 40)
      tree
      (recur base-r))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:origin (cq/rel-vec 0.5 0.5)
   :root (build-tree (cq/rel-h 0.125))
   :t 0.0})

(defn update-state [state]
  (assoc state :t (/ (q/millis) 1000.0)))

(defn draw-joint [element]
  (q/no-stroke)
  (q/fill 0.0 1.0)
  (cq/circle element 2.0))

(defn draw-connector [parent element color]
  (q/stroke-weight 2.0)
  (apply q/stroke color)
  (q/line parent element))

(defn draw [{:keys [origin root t]}]
  (q/background 1.0 1.0)
  (draw-joint origin)
  (doseq [group (apply map vector
                       (map (partial plot-elements {:position origin :angle 0} root)
                            (map + (range 0.0 0.1 (/ 0.1 4)) (repeat t))))]
    (let [[parent element draw _color t] (first group)]
      (when draw
        (draw parent element t)))
    (doseq [[parent element _draw color _t] group]
      (draw-joint element)
      (draw-connector parent element color))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.readable-width
    [:p "Randomly generate a tree structure with each node either at a fixed
    angle, orbiting or swinging on a pendulum between two random angles from
    it's parent. Angles can be either relative to the parent or locked to screen
    coordinates. Orbital and pendulum radial length is usually fixed, but a
    small percentage oscillate. For some transitions, it acts like a clock,
    easing from a steady state to the next for a fixed number of steps per
    period of rotation."]
    [:p "Each frame of animation renders the current position at
    time " [:code "t"] ", as well as it's future positions
    at " [:code "t+0.025"] ", " [:code "t+0.05"] ", and " [:code "t+0.075"] " in
    seconds."]]])

(sketch/definition kinetic-elliptics
  {:created-at "2023-09-15"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

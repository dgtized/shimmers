(ns shimmers.sketches.bubbles
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-bubble []
  (gc/circle (cq/rel-pos (dr/random) 1.0)
             (+ 1 (dr/random-int 6))))

(defn setup []
  {:bubbles []})

(defn in-bounds? [c]
  (<= 0 (get-in c [:p :y]) (q/height)))

(defn update-bubble [bubble]
  ;; consider adding acc/vel and horizontal motion from wind?
  (when (in-bounds? bubble)
    (g/translate bubble (gv/vec2 0 (- (/ 0.33 (:r bubble)))))))

(defn combine-bubble [a b]
  (gc/circle
   (tm/mix (:p a) (:p b)
           (/ (if (> (:r a) (:r b)) (:r b) (:r a))
              (+ (:r a) (:r b))))
   (math/sqrt (+ (math/pow (:r a) 2)
                 (math/pow (:r b) 2)))))

(defn combine-intersecting [bubbles]
  (loop [ordered (sort-by (comp :p :x) bubbles)
         results []]
    (if (empty? ordered)
      results
      (let [[a & xs] ordered
            b (some (fn [b] (when (g/intersect-shape a b) b)) xs)]
        (if b
          (recur (conj (remove #{b} xs) (combine-bubble a b)) results)
          (recur xs (conj results a)))))))

(defn update-state [{:keys [bubbles] :as state}]
  (let [active (keep update-bubble (combine-intersecting bubbles))]
    (assoc state :bubbles
           (if (and (dr/chance 0.03)
                    (< (count active) 512))
             (conj active (make-bubble))
             active))))

(defn draw [{:keys [bubbles]}]
  ;; color cycle for streaking effect
  ;; TODO: consider just cycling across predefined color curve?
  (let [fc (q/frame-count)]
    (when (zero? (mod fc 4))
      (q/background (+ 150 (* 100 (q/noise 0 (/ fc 360))))
                    (+ 150 (* 100 (q/noise 64 (/ fc 540))))
                    (+ 150 (* 100 (q/noise 128 (/ fc 720))))
                    8)))
  (q/no-fill)
  (q/stroke-weight 0.05)
  (q/stroke 40 40 240 96)
  (q/ellipse-mode :radius)
  (doseq [bubble bubbles]
    (cq/circle bubble)))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition bubbles
  {:created-at "2021-02-02"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

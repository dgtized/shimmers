(ns shimmers.sketches.lattice-reflections
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  {:radius (cq/rel-h 0.02)
   :plans (->> (fn []
                 (let [nested (dr/chance 0.66)]
                   {:n (dr/random-int 3 13)
                    :rate (tm/clamp (dr/gaussian 1 0.33) 0.05 1.95)
                    :nested nested
                    :filtered (dr/weighted {even? 1.0
                                            (constantly true) 1.0})}))
               (repeatedly 6)
               (map-indexed (fn [i r] (assoc r :idx i))))})

(defn update-state [state]
  state)

(defn surround [{:keys [p r]} {:keys [n rate phase]} t]
  (let [sv (math/sin (/ tm/PI n))
        r' (/ (* r sv) (- 1 sv))
        circle (gc/circle p (+ r r'))]
    (for [s (butlast (tm/norm-range n))]
      (gc/circle (g/point-at circle (+ s (* rate t) phase)) r'))))

(defn even [x]
  (let [x' (int x)]
    (cond (even? x') x'
          (< x' x)
          (inc x')
          :else
          (dec x'))))

(comment (mapv (fn [x] [x (even x)]) [3.2 3.6 4.3 4 5.3]))

(defn circles [radius plans t]
  (loop [circles [(gc/circle radius)] r radius plans plans]
    (if (seq plans)
      (let [lcircle (gc/circle r)
            {:keys [n] :as plan} (first plans)
            additions (surround lcircle plan t)
            r' (:r (first additions))]
        (recur
         (concat circles
                 (conj (map (fn [c]
                              (if (:nested plan)
                                (let [v (even (* tm/PHI n))]
                                  (vary-meta c assoc
                                             :nested v
                                             :filtered (:filtered plan)))
                                c))
                            additions)
                       (vary-meta lcircle assoc :lcircle true)))
         (+ r (* 2 r'))
         (rest plans)))
      circles)))

(defn features [circles radius t]
  (let [s (* 0.11 (math/sin t))]
    (mapcat
     (fn [c]
       (cond
         (:lcircle (meta c))
         []
         (< (:r c) (* 1.1 radius))
         [(g/scale-size c (+ 0.5 s))]
         :else
         (concat
          [(g/scale-size c (+ 0.66 s))
           (g/scale-size c (- 0.33 s))]
          (let [m (meta c)]
            (when-let [nested (:nested m)]
              (keep-indexed
               (fn [i x] (when ((:filtered m) i) x))
               (surround (g/scale-size c (- 0.33 s))
                         {:n nested :rate (* 0.5 s) :phase 0}
                         t)))))))
     circles)))

(defn draw [{:keys [radius plans]}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  ;; TODO add translation drift over time
  (q/translate (cq/rel-vec 0.5 0.5))
  (let [t (/ (q/millis) 3000.0)
        radius' (* radius (+ 1 (* 0.5 (eq/unit-sin t))))
        circles (circles radius' plans (/ t 10.0))]
    (doseq [obj (concat circles (features circles radius' t))]
      (qdg/draw obj))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.evencols
    [:p.readable-width
     "Experimenting with fitting N circles perfectly around an inner circle for K iterations."]]])

(sketch/definition lattice-reflections
  {:created-at "2026-05-16"
   :tags #{}
   :type :quil}
  (ctrl/mount page))

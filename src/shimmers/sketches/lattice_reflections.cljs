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
   :rules (->> (fn []
                 {:n (dr/random-int 3 13)
                  :phase (tm/clamp (dr/gaussian 1 0.4) 0.05 1.95)})
               (repeatedly 6)
               (map-indexed (fn [i r] (assoc r :idx i))))})

(defn update-state [state]
  state)

(defn surround [{:keys [p r]} n phase t]
  (let [sv (math/sin (/ tm/PI n))
        r' (/ (* r sv) (- 1 sv))
        circle (gc/circle p (+ r r'))]
    (for [s (butlast (tm/norm-range n))]
      (gc/circle (g/point-at circle (+ s (* phase t))) r'))))

(defn even [x]
  (let [x' (int x)]
    (cond (even? x') x'
          (< x' x)
          (inc x')
          :else
          (dec x'))))

(comment (odd 3.2) (odd 3.6) (odd 4.3) (odd 4) (odd 5.3))

(defn circles [radius rules t]
  (loop [circles [(gc/circle radius)] r radius rules rules]
    (if (seq rules)
      (let [lcircle (gc/circle r)
            {:keys [idx n phase]} (first rules)
            additions (surround lcircle n phase t)]
        (recur
         (concat circles
                 (conj (map (fn [c]
                              (if (even? idx)
                                (let [v (even (* tm/PHI n))]
                                  (vary-meta c assoc :nested v))
                                c))
                            additions)
                       (vary-meta lcircle assoc :lcircle true)))
         (+ r (* 2 (:r (first additions))))
         (rest rules)))
      circles)))

;; FIXME: support both odd or even subdivisions not even only
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
         (concat [(g/scale-size c (+ 0.66 s))
                  (g/scale-size c (- 0.33 s))]
                 (if (:nested (meta c))
                   (keep-indexed
                    (fn [i x] (when (even? i) x))
                    (surround (g/scale-size c (- 0.33 s)) (:nested (meta c)) (* 0.5 s) t))
                   []))))
     circles)))

(defn draw [{:keys [radius rules]}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  ;; TODO add translation drift over time
  (q/translate (cq/rel-vec 0.5 0.5))
  (let [t (/ (q/millis) 3000.0)
        radius' (* radius (+ 1 (* 0.5 (eq/unit-sin t))))
        circles (circles radius' rules (/ t 10.0))]
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

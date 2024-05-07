(ns shimmers.math.wobble
  (:require
   [clojure.math :as math]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]))

(defn create
  ([r] (create r (dr/random-tau)))
  ([r c] {:r r :c c}))

(defn sin [{:keys [r c]} p t]
  (math/sin (+ (* r t) p c)))

(defn cos [{:keys [r c]} p t]
  (math/sin (+ (* r t) p c)))

(defn tsin [{:keys [r c]} p t]
  (math/sin (* eq/TAU (+ (* r t) p c))))

(defn tcos [{:keys [r c]} p t]
  (math/cos (* eq/TAU (+ (* r t) p c))))

(defn cube-sin [osc p t]
  (eq/cube (sin osc p t)))

(defn cube-tsin [osc p t]
  (eq/cube (tsin osc p t)))

(defn cube-cos [osc p t]
  (eq/cube (cos osc p t)))

(defn cube-tcos [osc p t]
  (eq/cube (tcos osc p t)))

(defn R [^double f ^double p ^double a ^double s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn O ^double [^double f ^double p ^double s]
  (math/sin (* eq/TAU (+ (* s f) p))))

(defn osc [f p v d s]
  (+ v (* d (math/sin (* eq/TAU (+ (* s f) p))))))

(defn create-osc [f v d]
  {:f f :v v :d d
   :fe (fn [p s] (osc f p v d s))})
